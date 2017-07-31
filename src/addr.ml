open Core
open Async

(* type t = Socket.Address.Inet.t  *)

include Socket.Address.Inet
(* 
let create = Socket.Address.Inet.create

let to_string = Socket.Address.Inet.to_string

let port = Socket.Address.Inet.port 

let addr = Socket.Address.Inet.addr 
 *)
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