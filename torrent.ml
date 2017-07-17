open Core
open Async
open Log.Global

module B = Bencode

type file_info = {
  name : string;
  length : int;
}

type t = {
  info_hash : Bt_hash.t;
  announce : string;
  announce_list : string list list;
  piece_length : int;
  pieces_hash : Bt_hash.t Array.t;
  mode : [`Single_file | `Multiple_file];
  files_info : file_info list
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
  let f i = String.sub s (i * split_size) split_size in
  Array.init (n / split_size) ~f

let from_chan chan =
  let bc = B.decode (`Channel chan) in 
  debug "torrent file = %s" (B.pretty_print bc);
  let announce_bc = get (B.dict_get bc "announce") in
  let announce = get (B.as_string announce_bc) in
  let announce_list : string list list =
    match B.dict_get bc "announce-list" with  
    | None -> []
    | Some al -> 
      let al : Bencode.t list = get (B.as_list al) in
      let f (x:Bencode.t) : string list = 
        let x = get (B.as_list x) in
        List.map x ~f:(fun x -> get (B.as_string x))
      in
      List.map al ~f
  in

  let info_dict_bc = get (B.dict_get bc "info") in 
  let info_str = B.encode_to_string info_dict_bc in 
  let pieces_bc = get (B.dict_get info_dict_bc "pieces") in
  let pieces = get (B.as_string pieces_bc) in
  let piece_length_bc = get (B.dict_get info_dict_bc "piece length") in
  let piece_length = get (B.as_int piece_length_bc) in
  let info_hash = Bt_hash.of_string (Sha1.to_bin (Sha1.string info_str)) in
  let pieces_hash = Array.map (split pieces hash_length) ~f:Bt_hash.of_string in 
  match B.dict_get info_dict_bc "length" with
  | Some length_bc ->
    let mode = `Single_file in
    let length = get (B.as_int length_bc) in
    let name_bc = get (B.dict_get info_dict_bc "name") in
    let name = get (B.as_string name_bc) in 
    let files_info = [{name; length}] in
    { mode; announce; info_hash; piece_length; pieces_hash; announce_list; 
      files_info }
  | None -> 
    let mode = `Multiple_file in
    let files_bc = get (B.dict_get info_dict_bc "files") in
    let files = get (B.as_list files_bc) in 
    let f (file_info_bc:Bencode.t) : file_info  =
      let name_bc = get (B.dict_get file_info_bc "path") in
      let name_list = get (B.as_list name_bc)  in 
      let names = List.map name_list ~f:(fun n -> get (B.as_string n)) in
      let length_bc = get (B.dict_get file_info_bc "length") in
      let length = get (B.as_int length_bc) in
      { name = Filename.of_parts names; length }
    in
    let files_info = List.map files f in
    { mode; announce; info_hash; piece_length; pieces_hash; announce_list; 
      files_info }
