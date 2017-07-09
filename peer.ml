open Core
open Async
open Log.Global


(** TODO see how to make a better use of buffers:
  - now we allocate a buffer for each send/get. Can we have one per-peer buffer?
  - we use two buffers a string, and a Bin_prot.Common buffer... can we use
    only one *)

type t = {
  mutable choked : bool; 
  mutable interested : bool;
  peer : Socket.Address.Inet.t;
  mutable id : string;
  reader : Reader.t;
  writer : Writer.t;
  have : Bitset.t
 }

exception Handshake_error

let create peer ~piece_num = 
  let wtc = Tcp.to_inet_address peer in
  info "trying to connect to peer %s" (Socket.Address.Inet.to_string peer);
  try_with (function () -> Tcp.connect wtc)
  >>| function
  | Ok (_, r, w) -> 
      Ok { peer; have = Bitset.create piece_num; id = ""; interested = false; choked = true; reader = r; writer = w}
  | Error err -> Error err

let handshake = "\019BitTorrent protocol"

let handshake sha this_peer_id = "\019BitTorrent protocol\000\000\000\000\000\000\000\000" 
                    ^ sha ^ this_peer_id

let handshake st info_sha this_peer_id =
  let handshake = handshake info_sha this_peer_id in 
  Writer.write st.writer handshake; 
  let hs_len = 68 in
  let sha_len = 20 in
  let info_pos = 48 in 
  let peer_pos = 48 in 
  let buf = String.create hs_len in
  Reader.really_read st.reader ~len:hs_len buf
  >>| function 
  | `Ok -> 
    let info_sha_rep = String.sub buf ~pos:info_pos ~len:sha_len in
    let peer_id = String.sub buf ~pos:peer_pos ~len:sha_len in
    if (info_sha_rep = info_sha) then 
      Error Handshake_error
    else ( 
      st.id <- peer_id;
      Ok ()
    ) 
  | `Eof _ -> Error Handshake_error

(* TODO there must be an API function to do that *)
let length_from_buf buf =
  let b0 = int_of_char (String.get buf 0) in
  let b1 = int_of_char (String.get buf 1) in
  let b2 = int_of_char (String.get buf 2) in
  let b3 = int_of_char (String.get buf 3) in
  b0 * 256 * 256 * 256 + b1 * 256 * 256  + b2 * 256 + b3

let get_message st =
  let buf = String.create 40000 in  (* TODO see what buffer size is enough, see if we can use a global buffer *)
  (* first 4 bytes contain size of message *) 
  Reader.really_read st.reader ~pos:0 ~len:4 buf  
  >>= function
  | `Eof _ -> assert false
  | `Ok ->
    let len = length_from_buf buf in
    debug "decoding message of (payload) length %d" len;
    Reader.really_read st.reader ~pos:4 ~len buf 
    >>| function
    | `Eof _ -> assert false 
    | `Ok -> 
      let com_buf = Bin_prot.Common.create_buf (len + 4) in
      (* copy buf content to a com_buf in order to use Bin_prot functions *)
      (* TODO seems inefficient... *)
      Bin_prot.Common.blit_string_buf buf com_buf ~len:(len+4);
      let pos_ref = ref 0 in
      Message.bin_read_t com_buf ~pos_ref

let send_message (x:t) (m:Message.t) =
  let w = x.writer in
  let len = Message.size m in 
  debug "sending message of len = %d" len;
  let com_buf = Bin_prot.Common.create_buf len in
  let pos = Message.bin_write_t com_buf 0 m in
  assert(pos = len); 
  let buf = Bytes.create len in 
  Bin_prot.Common.blit_buf_string com_buf buf ~len:len;
  Writer.write w buf;
  return ()

let to_string x = Socket.Address.Inet.to_string x.peer

