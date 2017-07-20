open Core
open Async
open Log.Global


(** TODO see how to make a better use of buffers. We allocate a
    buffer for each send/get. Can we have one per-peer buffer? 

    A better solution for send_message/get_message would be to use
    the bin_prot_read and bin_prot_write but the current version
    of the library uses a fixed 8-bytes length (4 bytes in bittorrent) *) 

type t = {
  mutable peer_choking : bool; 
  mutable peer_interested : bool;
  mutable am_choking : bool;
  mutable am_interested : bool; 
  peer_addr : Socket.Address.Inet.t;
  mutable id : Peer_id.t;
  reader : Reader.t;
  writer : Writer.t;
  have : Bitset.t;
  mutable pending : Int.Set.t;
  mutable time_since_last_reception : int;
  mutable time_since_last_send : int;
  mutable idle : bool;
}

(* TODO simply use option types for errors? *)

let create peer_addr ~piece_num = 
  let wtc = Tcp.to_inet_address peer_addr in
  debug "trying to connect to peer %s" (Socket.Address.Inet.to_string peer_addr);
  try_with (function () -> Tcp.connect wtc)
  >>| function
  | Ok (_, r, w) -> 
    Ok {
      peer_addr; 
      have = Bitset.empty piece_num; 
      id = Peer_id.dummy;
      peer_interested = false; 
      peer_choking = true; 
      am_interested = false;
      am_choking = true; 
      reader = r; 
      writer = w; 
      pending = Int.Set.empty;
      time_since_last_reception = 0; 
      time_since_last_send = 0; 
      idle = false;
    }
  | Error err -> Error err

let to_string t = Socket.Address.Inet.to_string t.peer_addr

exception Handshake_error

let hs hash pid = 
  sprintf "\019BitTorrent protocol\000\000\000\000\000\000\000\000%s%s" hash pid   

let hs_len = 68
let hash_len = 20 
let info_pos = 48 
let peer_pos = 48 

let handshake t hash pid =
  let hash = Bt_hash.to_string hash in
  let pid = Peer_id.to_string pid in
  let hs = hs hash pid in

  Writer.write t.writer hs; 

  let buf = String.create hs_len in
  Reader.really_read t.reader ~len:hs_len buf
  >>| function 
  | `Ok -> 
    let info_hash_rep = String.sub buf ~pos:info_pos ~len:hash_len in
    let remote_peer_id = String.sub buf ~pos:peer_pos ~len:hash_len in
    if info_hash_rep = hash then 
      Error Handshake_error
    else ( 
      t.id <- Peer_id.of_string remote_peer_id;
      Ok ()
    ) 
  | `Eof _ -> Error Handshake_error

let get_message t =
  (* this should be big enough to contain [Piece.block_size]
     and the message header TODO *)
  let buf = Bin_prot.Common.create_buf 40000 in  
  (* we need to get the prefix length first to know how many bytes to
     read *)
  let prefix_len_substr = Bigsubstring.create buf ~pos:0 ~len:4 in 
  match%bind Reader.really_read_bigsubstring t.reader prefix_len_substr with
  | `Eof _ -> return `Eof
  | `Ok -> (
      let pos_ref = ref 0 in
      let len = Bin_prot.Read.bin_read_network32_int buf ~pos_ref in  
      let msg_substr = Bigsubstring.create buf ~pos:4 ~len in 
      Reader.really_read_bigsubstring t.reader msg_substr
      >>| function
      | `Eof _ -> 
        info "Didn't get message - peer %s closed connection" (to_string t);
        `Eof 
      | `Ok -> 
        t.time_since_last_reception <- 0;
        pos_ref := 0;
        let msg = Message.bin_read_t buf ~pos_ref in
        debug "got message %s from %s" (Message.to_string msg) (to_string t);
        `Ok msg)

let send_message t (m:Message.t) =
  let len = 4 + Message.size m in (* prefix length + message *)
  debug "sending message %s %s" (Message.to_string m) (to_string t);
  let buf = Bin_prot.Common.create_buf len in
  let pos = Message.bin_write_t buf 0 m in
  assert(pos = len); 
  t.time_since_last_send <- 0;
  Writer.write_bigstring t.writer buf

let has_piece t i = Bitset.belongs t.have i

let owned_pieces t = t.have

let set_owned_piece t i = Bitset.insert t.have i

let set_owned_pieces t s = Bitset.insert_from_bitfield t.have s;
  info "Peer %s has %d pieces" (to_string t) (Bitset.card t.have) 

let time_since_last_received_message t = t.time_since_last_reception

let is_idle t = t.idle

let set_peer_interested t b = t.peer_interested <- b

let set_peer_choking t b = t.peer_choking <- b

let set_am_interested t b = t.am_interested <- b

let set_am_choking t b = t.am_choking <- b

let is_peer_choking t = t.peer_choking

let is_peer_interested t = t.peer_interested

let am_choking t = t.am_choking

let am_interested t = t.am_interested

let set_idle t b = t.idle <- b

let pending_size t = Int.Set.length t.pending

let has_pending t = not (Int.Set.is_empty t.pending)

let clear_pending t = t.pending <- Int.Set.empty 

let remove_pending t i = t.pending <- Int.Set.remove t.pending i

let add_pending t i = t.pending <- Int.Set.add t.pending i

let iter_pending t ~f = Int.Set.iter t.pending ~f

let validate t c = assert c

let stats t = 
  info "peer %s: idle/choking/interested %B %B %B" 
  (to_string t) t.idle t.peer_choking t.peer_interested 

let tick t =
  t.time_since_last_send <- t.time_since_last_send + 1;
  t.time_since_last_reception <- t.time_since_last_reception + 1;
  let is_ka = t.time_since_last_send >= Global.keep_alive in 
  let is_id = t.time_since_last_reception >= Global.idle in 
  match (is_ka, is_id) with 
  | (_, true) -> 
    let l = Int.Set.to_list t.pending in 
    set_idle t true;
    clear_pending t;
    `Idle l 
  | (true, _) -> `Keep_alive
  | (false, false) -> 
     set_idle t false;
     `Ok













