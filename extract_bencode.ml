open Core
open Async
open Log.Global

module B = Bencode

type torrent_info = {
  name : string;
  info_hash : string;
  announce : string;
  piece_length : int;
  pieces_hash : string Array.t;
  length : int;
}

let hash_length = 20

exception Wrong_Format

let get x =
  match x with
  | Some y -> y
  | None -> raise Wrong_Format

let split (s:string) split_size =
  let n = String.length s in
  assert (n % split_size = 0);
  Array.init (n / split_size) ~f:(fun i -> String.sub s (i * split_size) split_size)

let from_torrent chan =
  let bc = B.decode (`Channel chan) in 
  (* debug "Torrent file bencode decoded = %s" (B.pretty_print bc); *)
  let announce_bc = get (B.dict_get bc "announce") in
  let announce = get (B.as_string announce_bc) in
  let info_dict_bc = get (B.dict_get bc "info") in 
  let info_str = B.encode_to_string info_dict_bc in 
  let pieces_bc = get (B.dict_get info_dict_bc "pieces") in
  let pieces = get (B.as_string pieces_bc) in
  let length_bc = get (B.dict_get info_dict_bc "length") in
  let length = get (B.as_int length_bc) in
  let piece_length_bc = get (B.dict_get info_dict_bc "piece length") in
  let piece_length = get (B.as_int piece_length_bc) in
  let name_bc = get (B.dict_get info_dict_bc "name") in
  let name = get (B.as_string name_bc) in 
  let info_hash = Sha1.to_bin (Sha1.string info_str) in
  let pieces_hash = split pieces hash_length in 
  { name; announce; info_hash; piece_length; pieces_hash; length; }

type tracker_reply = {
  complete : int;
  incomplete : int;
  interval : int;
  peers : Socket.Address.Inet.t list
}

(* let f s =
    let pos = 0 in
    let (acc : Int32.t ref) = ref 0l in
    let (port : int ref) = ref 0 in
    for i = pos to pos + 3 do
      let byte = Int32.of_int_exn (int_of_char s.[i]) in
      let open Int32 in
      acc := !acc * 256l;
      acc := !acc + byte
    done;
    for i = pos + 4 to pos + 5 do
      port := !port * 256;
      port := !port + int_of_char s.[i]
    done;
    let addr = Unix.Inet_addr.inet4_addr_of_int32 !acc in
    Socket.Address.Inet.create addr !port
*)

let rec decode_peers s =
  let ar = split s 6 in
  let compact_repr (s:string) : Socket.Address.Inet.t =
    let addr_int32 = Binary_packing.unpack_signed_32 ~byte_order:`Big_endian ~buf:s ~pos:0 in
    let port = Binary_packing.unpack_unsigned_16_big_endian ~pos:4 ~buf:s in
    let addr = Unix.Inet_addr.inet4_addr_of_int32 addr_int32 in
    Socket.Address.Inet.create addr port
  in 
  Array.to_list (Array.map ar ~f:compact_repr)


let from_tracker_reply s =
  let bc = B.decode (`String s) in 
  (* debug "Tracker reply = %s" (B.pretty_print bc); *)
  let complete = get ((B.as_int (get (B.dict_get bc "complete")))) in
  let incomplete = get ((B.as_int (get (B.dict_get bc "incomplete")))) in
  let interval = get ((B.as_int (get (B.dict_get bc "interval")))) in
  let peers_str = get (B.as_string (get (B.dict_get bc "peers"))) in
  let peers = decode_peers peers_str in
  { complete; incomplete; interval; peers; }


