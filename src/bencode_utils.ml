open Core
open Async

exception Bencode_error

let get x =
  match x with
  | Some y -> y
  | None -> raise Bencode_error

let split (s:string) split_size =
  let n = String.length s in
  assert (n % split_size = 0);
  let f i = String.sub s (i * split_size) split_size in
  Array.init (n / split_size) ~f

(* TODO rewrite this *)
let encode_peer peer_addr = 
  let port = Socket.Address.Inet.port peer_addr in
  let s = "  " in
  Binary_packing.pack_signed_16 ~byte_order:`Big_endian ~buf:s ~pos:0 port;
  "\127\000\000\001" ^ s

let decode_peer (s:string) : Socket.Address.Inet.t =
  let addr_int32 = Binary_packing.unpack_signed_32 ~byte_order:`Big_endian 
      ~buf:s ~pos:0 in
  let port = Binary_packing.unpack_unsigned_16_big_endian ~pos:4 ~buf:s in
  let addr = Unix.Inet_addr.inet4_addr_of_int32 addr_int32 in
  Socket.Address.Inet.create addr port