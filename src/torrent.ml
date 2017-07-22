open Core
open Async
open Log.Global

module B = Bencode

type t = {
  info_hash : Bt_hash.t;
  announce : string;
  announce_list : string list list;
  piece_length : int;
  pieces_hash : Bt_hash.t Array.t;
  files_info : (string * int) list; 
  torrent_name : string;
  total_length : int;
  num_pieces : int;
  num_files : int;
}

let from_file f =
  let open Bencode_utils in
  let chan = In_channel.create f in 
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
  let pieces_hash = Array.map (split pieces Bt_hash.length) ~f:Bt_hash.of_string in 
  let num_pieces = Array.length pieces_hash in
  let torrent_name = f in
  let files_info = (
    match B.dict_get info_dict_bc "length" with
    | Some length_bc ->
      let length = get (B.as_int length_bc) in
      let name_bc = get (B.dict_get info_dict_bc "name") in
      let name = get (B.as_string name_bc) in 
      [name, length]
    | None -> 
      let files_bc = get (B.dict_get info_dict_bc "files") in
      let files = get (B.as_list files_bc) in 
      let f (file_info_bc: Bencode.t) : string * int =
        let name_bc = get (B.dict_get file_info_bc "path") in
        let name_list = get (B.as_list name_bc)  in 
        let names = List.map name_list ~f:(fun n -> get (B.as_string n)) in
        let length_bc = get (B.dict_get file_info_bc "length") in
        let length = get (B.as_int length_bc) in
        (Filename.of_parts names), length
      in
      List.map files f) 
  in 
  let num_files = List.length files_info in 
  let total_length = List.fold files_info ~init:0 ~f:(fun acc (_,l) -> l + acc)  in

  info "torrent: %s" torrent_name;
  info "torrent: %d files" num_files;
  info "torrent: %d pieces" num_pieces;
  info "torrent: piece length = %d" piece_length;

  { announce; info_hash; piece_length; pieces_hash; announce_list; 
    files_info; torrent_name; total_length; num_pieces; num_files }
