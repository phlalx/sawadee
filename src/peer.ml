open Core
open Async
open Log.Global

(** A better solution for send_message/get_message would be to use
    bin_prot_read and bin_prot_write but unfortunately the current version
    of the library uses a fixed 8-bytes length (4 bytes in bittorrent). *) 

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
  mutable idle : bool;
  kind : [`Am_initiating | `Peer_initiating ];
  receive_buffer : Bigstring.t;
  send_buffer : Bigstring.t;  
  mutable downloading : bool;
  mutable uploading : bool;
} 

let buffer_size = Message.max_size

let equals t1 t2 = t1.id = t2.id

let peer_id t = t.id 

let create peer_addr r w kind =
  (* this needs to be done so as we don't get errors when remote peers
     closes his connection *)
  Writer.set_raise_when_consumer_leaves w false;
  {
    peer_addr; 
    have = Bitset.empty 0; (* to be set by [init_size_owned_pieces] *)
    id = Peer_id.dummy;
    peer_interested = false; 
    peer_choking = true; 
    am_interested = true; (* should be opposite when starting *)
    am_choking = false; (* should be opposite when starting TODO *) 
    reader = r; 
    writer = w; 
    pending = Int.Set.empty;
    idle = false;
    kind;
    (* this should be big enough to contain [Piece.block_size]
       and the message header TODO *)
    receive_buffer = Bin_prot.Common.create_buf 40000;
    send_buffer = Bin_prot.Common.create_buf 40000;
    downloading = false;
    uploading = false;
  }

let to_string t = sprintf "%s" (Peer_id.to_readable_string t.id)

(* last bit of sequence set to 1 = DHT support *)
let hs_prefix = "\019BitTorrent protocol\000\000\000\000\000\000\000\001"  

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
  let buf = t.receive_buffer in 
  (* we need to get the prefix length first to know how many bytes to
     read *)
  (* TODO doesn't seem right... I'm not sure this is how bin_prot.read
     is supposed to work. Do we really need to peek the size first  *)
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
        pos_ref := 0;
        let msg = Message.bin_read_t buf ~pos_ref in
        debug "got message %s from %s" (Message.to_string msg) (to_string t);
        `Ok msg)

let send_message t (m:Message.t) =
  let len = 4 + Message.size m in (* prefix length + message *)
  debug "sending message %s %s" (Message.to_string m) (to_string t);
  let buf = t.send_buffer in
  let pos = Message.bin_write_t buf 0 m in
  assert(pos = len); 
  Writer.write_bigstring t.writer buf ~len

let has_piece t i = Bitset.belongs t.have i

let owned_pieces t = t.have

let set_owned_piece t i = Bitset.insert t.have i

let set_owned_pieces t s = Bitset.insert_from_bitfield t.have s;
  info "peer %s has %d pieces" (to_string t) (Bitset.card t.have) 

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

let get_pending t = Int.Set.to_list t.pending

(* must be call right after handshake *)
let init_size_owned_pieces t num_piece = t.have <- Bitset.empty num_piece

let addr_to_string t = Socket.Address.Inet.to_string t.peer_addr

let set_downloading t = 
  if not t.downloading then (
    Print.printf  "downloading from peer %s\n" (addr_to_string t);
    t.downloading <- true;
  )

let set_uploading t = 
  if not t.uploading then (
    Print.printf  "uploading to peer %s\n" (addr_to_string t);
    t.uploading <- true;
  )


let addr t = Socket.Address.Inet.addr t.peer_addr


