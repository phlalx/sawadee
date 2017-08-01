open Core
open Async
open Log.Global

(** A better solution for send_message/get_message would be to use
    bin_prot_read and bin_prot_write but unfortunately the current version
    of the library uses a fixed 8-bytes length (4 bytes in bittorrent). *) 

type t = {
  peer_addr : Addr.t;
  mutable id : Peer_id.t;
  reader : Reader.t;
  writer : Writer.t;
  receive_buffer : Bigstring.t;
  send_buffer : Bigstring.t;  
  mutable downloading : bool;
  mutable uploading : bool;
} 

let buffer_size = Message.max_size

let equals t1 t2 = t1.id = t2.id

let id t = t.id 

let create peer_addr r w =
  (* this needs to be done so as we don't get errors when remote peers
     closes his connection *)
  Writer.set_raise_when_consumer_leaves w false;
  {
    peer_addr; 
    id = Peer_id.dummy;
    reader = r; 
    writer = w; 
    (* this should be big enough to contain [Piece.block_size]
       and the message header TODO *)
    receive_buffer = Bin_prot.Common.create_buf 40000;
    send_buffer = Bin_prot.Common.create_buf 40000;
    downloading = false;
    uploading = false;
  }

let to_string t = Printf.sprintf "%s" (Peer_id.to_readable_string t.id)

(* last bit of sequence set to 1 = DHT support *)
let hs_prefix = "\019BitTorrent protocol\000\000\000\000\000\016\000\001"  

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
          (Addr.to_string t.peer_addr);
        Ok ()
    ) 
  | `Eof _ -> Error (Error.of_string "handshake error")

let extract_reply received =
  let hash_pos = String.length hs_prefix  in
  let peer_pos = hash_pos + Bt_hash.length in
  let info_hash_rep = String.sub received ~pos:hash_pos ~len:Bt_hash.length in
  let remote_peer_id = String.sub received ~pos:peer_pos ~len:Peer_id.length in
  info_hash_rep, remote_peer_id

let wait_handshake t (has_hash : Bt_hash.t -> bool) pid =
  debug "handshake (wait) from %s" (to_string t);
  debug "trying to read %d bytes" hs_len;
  let buf = String.create hs_len in
  Reader.really_read t.reader buf ~len:hs_len 
  >>| function
  | `Ok -> ( 
      let info_hash_str, peer_id_str = extract_reply buf in
      let info_hash = Bt_hash.of_string info_hash_str in 
      if has_hash info_hash then (
        let peer_id = Peer_id.of_string peer_id_str in
        let pid_str = Peer_id.to_string pid in
        t.id <- peer_id;
        let hs = hs info_hash_str pid_str in
        Writer.write t.writer ~len:hs_len hs;
        info "handshake ok with %s" (to_string t);
        Ok info_hash 
      ) else 
        Error (Error.of_string "we don't serve this torrent")
    )
  | `Eof _ -> Error (Error.of_string "handshake error")

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

exception Incorrect_behavior

let validate t b = if not b then raise Incorrect_behavior

let addr_to_string t = Addr.to_string t.peer_addr

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

let addr t = Addr.addr t.peer_addr

let create_with_connect addr = 
    let open Deferred.Or_error.Monad_infix in 
    let wtc = Tcp.to_inet_address addr in
    debug "try connecting to peer %s" (Addr.to_string addr);
    Deferred.Or_error.try_with (function () -> Tcp.connect wtc)
    >>| fun (_, r, w) ->
    create addr r w 

let close t = 
  Writer.close t.writer >>= fun () -> Reader.close t.reader
