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

let to_bencode r =
  let open Bencode in 
  Dict [
    ("complete", Integer r.complete);
    ("incomplete", Integer r.incomplete);
    ("interval", Integer r.interval);
    ("peers", Bencode_utils.peers_to_bencode r.peers)
  ] |> Bencode.encode_to_string

let of_bencode s =
  let bc = B.decode (`String s) in 
  debug "tracker reply = %s" (B.pretty_print bc);
  let open Bencode_utils in
  let complete = get ((B.as_int (get (B.dict_get bc "complete")))) in
  let incomplete = get ((B.as_int (get (B.dict_get bc "incomplete")))) in
  let interval = get ((B.as_int (get (B.dict_get bc "interval")))) in
  let peers_bencode = get (B.dict_get bc "peers") in
  let peers = bencode_to_peers peers_bencode in
  { complete; incomplete; interval; peers; }

