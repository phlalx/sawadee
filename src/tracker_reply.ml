open Core
open Async
open Log.Global

module B = Bencode

exception Bencode_error

type t = {
  complete : int;
  incomplete : int;
  interval : int;
  peers : Socket.Address.Inet.t list
}

(* TODO rewrite this *)
let encode_peer peer_addr = 
  let port = Socket.Address.Inet.port peer_addr in
  let s = "  " in
  Binary_packing.pack_signed_16 ~byte_order:`Big_endian ~buf:s ~pos:0 port;
  "\127\000\000\001" ^ s

(* TODO rewrite this *)
let rec decode_peers s =
  let peer_addr_length = 6 in
  let ar = Bencode_utils.split s peer_addr_length in
  let compact_repr (s:string) : Socket.Address.Inet.t =
    let addr_int32 = Binary_packing.unpack_signed_32 ~byte_order:`Big_endian 
        ~buf:s ~pos:0 in
    let port = Binary_packing.unpack_unsigned_16_big_endian ~pos:4 ~buf:s in
    let addr = Unix.Inet_addr.inet4_addr_of_int32 addr_int32 in
    Socket.Address.Inet.create addr port
  in 
  Array.to_list (Array.map ar ~f:compact_repr)

let to_bencode r =
  let open Bencode in 
  let f acc p = acc ^ (encode_peer p) in
  let peers_str = List.fold r.peers ~init:"" ~f in 
  Dict [
    ("complete", Integer r.complete);
    ("incomplete", Integer r.incomplete);
    ("interval", Integer r.interval);
    ("peers", String peers_str)
  ] |> Bencode.encode_to_string

let of_bencode s =
  let bc = B.decode (`String s) in 
  debug "tracker reply = %s" (B.pretty_print bc);
  let open Bencode_utils in
  let complete = get ((B.as_int (get (B.dict_get bc "complete")))) in
  let incomplete = get ((B.as_int (get (B.dict_get bc "incomplete")))) in
  let interval = get ((B.as_int (get (B.dict_get bc "interval")))) in
  let peers_str = get (B.as_string (get (B.dict_get bc "peers"))) in
  let peers = decode_peers peers_str in
  { complete; incomplete; interval; peers; }

