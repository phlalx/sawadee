open Core
open Async
open Log.Global

type t = {
  name : string;
  info_sha1 : string;
  announce : string;
  pieces : string
}

exception Wrong_Format

let some_or_fail x =
  match x with
  | Some y -> y
  | None -> raise Wrong_Format

let extract_from_bencode chan =
  let bc = Bencode.decode (`Channel chan) in 
  debug "%s" (Bencode.pretty_print bc);
  let announce_bc = some_or_fail (Bencode.dict_get bc "announce") in
  let announce = some_or_fail (Bencode.as_string announce_bc) in
  let info_dict = some_or_fail (Bencode.dict_get bc "info") in 
  let info_str = Bencode.encode_to_string info_dict in 
  let pieces_bc = some_or_fail (Bencode.dict_get info_dict "pieces") in
  let pieces_str = some_or_fail (Bencode.as_string pieces_bc) in
  let name_bc = some_or_fail (Bencode.dict_get info_dict "name") in
  let name_str = some_or_fail (Bencode.as_string name_bc) in 
  let hash = Sha1.to_bin (Sha1.string info_str) in
  assert ((String.length hash) = 20);
  { 
    name = name_str;
    announce = announce;
    info_sha1 = hash;
    pieces = pieces_str
  }
