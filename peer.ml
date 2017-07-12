open Core
open Async
open Log.Global

(** TODO see how to make a better use of buffers. We allocate a
    buffer for each send/get. Can we have one per-peer buffer? 

    A better solution for send_message/get_message would be to use
    the bin_prot_read and bin_prot_write but the current version
    of the library uses a fixed 8-bytes length (4 bytes in bittorrent) *) 

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

let handshake hash this_peer_id = "\019BitTorrent protocol\000\000\000\000\000\000\000\000" 
                                  ^ hash ^ this_peer_id

let handshake t info_hash this_peer_id =
  let handshake = handshake info_hash this_peer_id in 
  Writer.write t.writer handshake; 
  let hs_len = 68 in
  let hash_len = 20 in
  let info_pos = 48 in 
  let peer_pos = 48 in 
  let buf = String.create hs_len in
  Reader.really_read t.reader ~len:hs_len buf
  >>| function 
  | `Ok -> 
    let info_hash_rep = String.sub buf ~pos:info_pos ~len:hash_len in
    let peer_id = String.sub buf ~pos:peer_pos ~len:hash_len in
    if info_hash_rep = info_hash then 
      Error Handshake_error
    else ( 
      t.id <- peer_id;
      Ok ()
    ) 
  | `Eof _ -> Error Handshake_error

let get_message t =
  (* this should be big enough to contain [Piece.block_size]
     and the message header *)
  let buf = Bin_prot.Common.create_buf 40000 in  
  (* we need to get the prefix length first to know how many bytes to
     read *)
  let prefix_len_substr = Bigsubstring.create buf ~pos:0 ~len:4 in 
  Reader.really_read_bigsubstring t.reader prefix_len_substr
  >>= function
  | `Eof _ -> return `Eof
  | `Ok -> (
      let pos_ref = ref 0 in
      let len = Bin_prot.Read.bin_read_network32_int buf ~pos_ref in  
      let msg_substr = Bigsubstring.create buf ~pos:4 ~len in 
      Reader.really_read_bigsubstring t.reader msg_substr
      >>| function
      | `Eof _ -> `Eof 
      | `Ok -> 
        pos_ref := 0;
        let msg = Message.bin_read_t buf ~pos_ref in
        `Ok msg)

let send_message t (m:Message.t) =
  let len = 4 + Message.size m in (* prefix length + message *)
  debug "sending message of len = %d" len;
  let buf = Bin_prot.Common.create_buf len in
  let pos = Message.bin_write_t buf 0 m in
  assert(pos = len); 
  Writer.write_bigstring t.writer buf

let to_string t = Socket.Address.Inet.to_string t.peer

