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
  mutable have : Bitset.t;
  mutable pending : Int.Set.t;
  mutable time_since_last_reception : int;
  mutable time_since_last_send : int;
  mutable idle : bool;
  kind : [`Am_initiating | `Peer_initiating ];
  buffer : Bigstring.t;
  (* sbuffer : Bigstring.t; TODO NOT WORKING *) 
}

let peer_id t = t.id 

let create peer_addr r w kind =
  Writer.set_raise_when_consumer_leaves w false;
  {
    peer_addr; 
    have = Bitset.empty 0; (* to be set by [init_size_owned_pieces] *)
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
    kind;
    buffer = Bin_prot.Common.create_buf 40000;
    (* sbuffer = Bin_prot.Common.create_buf 40000; *)
  }

let to_string t = 
   Printf.sprintf "%s" (Peer_id.to_readable_string t.id)

let hs_prefix = "\019BitTorrent protocol\000\000\000\000\000\000\000\000"  

let hs hash pid = sprintf "%s%s%s" hs_prefix hash pid   

let hs_len = (String.length hs_prefix) + Bt_hash.length + Bt_hash.length (* 68 *)

(* handshake:
   initiator sends 20 bytes prefix + info_hash + pid
   respondant sends same sequence with its own pid.
   hash must match.
*)

(* validate sequence received and extract peer id *)
let validate_handshake received info_hash =
  let hash_pos = String.length hs_prefix  in
  let peer_pos = hash_pos + Bt_hash.length in
  let info_hash_rep = String.sub received ~pos:hash_pos ~len:Bt_hash.length in
  let remote_peer_id = String.sub received ~pos:peer_pos ~len:Peer_id.length in
  match info_hash_rep = info_hash with
  | true -> Some remote_peer_id
  | false -> None

let initiate_handshake t hash pid =
  debug "handshake (initiate) with %s" (to_string t);
  let hash = Bt_hash.to_string hash in
  let pid = Peer_id.to_string pid in
  let hs = hs hash pid in

  Writer.write t.writer hs ~len:hs_len; 

  let buf = String.create hs_len in
  Reader.really_read t.reader ~len:hs_len buf
  >>| function 
  | `Ok -> ( 
      match validate_handshake buf hash with
      | None -> Error (Error.of_string "hash error")
      | Some p -> t.id <- Peer_id.of_string p;
        info "handshake ok with %s = %s" (to_string t)
         (Socket.Address.Inet.to_string t.peer_addr);
        Ok ()
    ) 
  | `Eof _ -> Error (Error.of_string "handshake error")

let wait_for_handshake t hash pid =
  debug "handshake (wait) from %s" (to_string t);
  let hash = Bt_hash.to_string hash in
  let pid = Peer_id.to_string pid in
  let hs = hs hash pid in
  let buf = String.create hs_len in
  debug "trying to read %d bytes" hs_len;
  Reader.really_read t.reader buf ~len:hs_len 
  >>| function
  | `Ok -> ( 
      match validate_handshake buf hash with
      | None -> Error (Error.of_string "hash error")
      | Some p -> 
        t.id <- Peer_id.of_string p;
        Writer.write t.writer ~len:hs_len hs;
        info "handshake ok with %s" (to_string t);
        Ok ()
    )
  | `Eof _ -> Error (Error.of_string "handshake error")

(* TODO: see if handshake is asynchronous, we may need only one function,
   otherwise factorize the common part *)
let handshake t hash pid =
  if t.kind = `Am_initiating then
    initiate_handshake t hash pid 
  else
    wait_for_handshake t hash pid

let get_message t =
  (* this should be big enough to contain [Piece.block_size]
     and the message header TODO *)
  let buf = t.buffer in 
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
  (* let buf = t.sbuffer in *)
  let pos = Message.bin_write_t buf 0 m in
  assert(pos = len); 
  t.time_since_last_send <- 0;
  Writer.write_bigstring t.writer buf

let has_piece t i = Bitset.belongs t.have i

let owned_pieces t = t.have

let set_owned_piece t i = Bitset.insert t.have i

let set_owned_pieces t s = Bitset.insert_from_bitfield t.have s;
  info "peer %s has %d pieces" (to_string t) (Bitset.card t.have) 

let time_since_last_received_message t = t.time_since_last_reception

let is_idle t = t.idle

let is_or_not b = if b then "" else "not"

let set_peer_interested t b = 
  info "%s is %s interested" (to_string t) (is_or_not b);
  t.peer_interested <- b

let set_peer_choking t b = 
  info "%s is %s choking" (to_string t) (is_or_not b);
  t.peer_choking <- b

let set_am_interested t b = 
  info "I am %s interested in %s" (is_or_not b) (to_string t);
  t.am_interested <- b

let set_am_choking t b = 
  info "I am %s choking %s" (is_or_not b) (to_string t);
  t.am_choking <- b

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

exception Incorrect_behavior

let validate t b = 
  if not b then raise Incorrect_behavior

let stats t = 
  info "** peer %s: idle/choking/interested %B %B %B" 
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

(* must be call right after handshake *)
let init_size_owned_pieces t num_piece = t.have <- Bitset.empty num_piece












