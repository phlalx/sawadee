open Core
open Async
open Log.Global

module B = Bencode_ext

exception Bencode_error

type t = {
  complete : int;
  incomplete : int;
  interval : int;
  peers : Addr.t list
}

let to_bencode r =
  B.Dict [
    ("complete", B.Integer r.complete);
    ("incomplete", B.Integer r.incomplete);
    ("interval", B.Integer r.interval);
    ("peers", Bencode_utils.peers_to_bencode r.peers)
  ] |> B.encode_to_string

let of_bencode s =

  let bc = B.decode (`String s) in 
  debug "tracker reply = %s" (B.pretty_print bc);
  let open Bencode_utils in
  let complete = B.get_int_from_dict_exn bc "complete" in
  let incomplete = B.get_int_from_dict_exn bc "incomplete" in
  let interval = B.get_int_from_dict_exn bc "interval" in
  let peers_bencode = B.dict_get_exn bc "peers" in
  let peers = bencode_to_peers peers_bencode in
  { complete; incomplete; interval; peers; }

