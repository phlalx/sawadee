open Core
open Async
open Log.Global

module B = Bencode

type torrent_info = {
  name : string;
  info_sha1 : string;
  announce : string;
  pieces : string;
  length : int
}

exception Wrong_Format

let get x =
  match x with
  | Some y -> y
  | None -> raise Wrong_Format

let from_torrent chan =
  let bc = B.decode (`Channel chan) in 
  debug "%s" (B.pretty_print bc);
  let announce_bc = get (B.dict_get bc "announce") in
  let announce = get (B.as_string announce_bc) in
  let info_dict_bc = get (B.dict_get bc "info") in 
  let info_str = B.encode_to_string info_dict_bc in 
  let pieces_bc = get (B.dict_get info_dict_bc "pieces") in
  let pieces = get (B.as_string pieces_bc) in
  let length_bc = get (B.dict_get info_dict_bc "length") in
  let length = get (B.as_int length_bc) in
  let name_bc = get (B.dict_get info_dict_bc "name") in
  let name = get (B.as_string name_bc) in 
  let info_sha1 = Sha1.to_bin (Sha1.string info_str) in
  { name; announce; info_sha1; pieces; length; }

type tracker_reply = {
  complete : int;
  incomplete : int;
  interval : int;
  peers : string
}

let from_tracker_reply s =
  let bc = B.decode (`String s) in 
  debug "%s" (B.pretty_print bc);
  let complete = get ((B.as_int (get (B.dict_get bc "complete")))) in
  let incomplete = get ((B.as_int (get (B.dict_get bc "incomplete")))) in
  let interval = get ((B.as_int (get (B.dict_get bc "interval")))) in
  let peers = get (B.as_string (get (B.dict_get bc "peers"))) in
  debug "%s" peers;
  { complete; incomplete; interval; peers; }
