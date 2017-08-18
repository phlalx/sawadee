open Core
open Async

module Be = Bencode_ext

include Socket.Address.Inet

let to_compact t = 
  let p = port t in
  let a = addr t in
  let a32 : Int32.t = Unix.Inet_addr.inet4_addr_to_int32_exn a in
  let s = String.create 6 in
  Binary_packing.pack_signed_32 ~byte_order:`Big_endian ~buf:s ~pos:0 a32;
  Binary_packing.pack_unsigned_16 ~byte_order:`Big_endian ~buf:s ~pos:4 p;
  s

let of_compact s =
  let addr_int32 = Binary_packing.unpack_signed_32 ~byte_order:`Big_endian 
      ~buf:s ~pos:0 in
  let port = Binary_packing.unpack_unsigned_16_big_endian ~pos:4 ~buf:s in
  let addr = Unix.Inet_addr.inet4_addr_of_int32 addr_int32 in
  create addr port

let of_bencode b = Be.as_string_exn b |> of_compact 

let to_bencode peer_addr = Be.String (to_compact peer_addr)

let list_to_bencode peers = 
  let f acc p = acc ^ (to_compact p) in Be.String (List.fold peers ~init:"" ~f)

let list_to_bencode_list peers = List.map peers ~f:to_bencode |> Be.List

let list_of_bencode_list b = Be.as_list_exn b |> List.map ~f:of_bencode

let length = 6

let list_of_bencode b = Be.split b length |> List.map ~f:of_bencode

let of_string s = String.split s ~on:':' |> function
 | [addr; p] -> create (Unix.Inet_addr.of_string addr) (int_of_string p)  
 | _ -> assert false

 let is_valid t = (port t) >= 1024