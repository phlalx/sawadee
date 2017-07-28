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

let rec decode_peers s =
  let peer_addr_length = 6 in
  let ar = Bencode_utils.split s peer_addr_length in
  Array.map ar ~f:Bencode_utils.decode_peer
  |> Array.to_list

let to_bencode r =
  let open Bencode in 
  let f acc p = acc ^ (Bencode_utils.encode_peer p) in
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

