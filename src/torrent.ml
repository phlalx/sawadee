open Core
open Async
open Log.Global

module B = Bencode_ext

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

let split (s:string) split_size =
  let n = String.length s in
  assert (n % split_size = 0);
  let f i = String.sub s (i * split_size) split_size in
  Array.init (n / split_size) ~f

let do_file torrent_name chan =
  let bc = B.decode (`Channel chan) in 
  debug "torrent file = %s" (B.pretty_print bc);
  let announce_bc = B.dict_get_exn bc "announce" in
  let announce = B.as_string_exn announce_bc in
  let announce_list : string list list =
    match B.dict_get bc "announce-list" with  
    | None -> []
    | Some al -> 
      let al : B.t list = B.as_list_exn al in
      let f (x:B.t) : string list = 
        let x = B.as_list_exn x in
        List.map x ~f:B.as_string_exn
      in
      List.map al ~f
  in

  let info_dict_bc = B.dict_get_exn bc "info" in 
  let info_str = B.encode_to_string info_dict_bc in 
  let pieces_bc = B.dict_get_exn info_dict_bc "pieces" in
  let pieces = B.as_string_exn pieces_bc in
  let piece_length_bc = B.dict_get_exn info_dict_bc "piece length" in
  let piece_length = B.as_int_exn piece_length_bc in
  let info_hash = Bt_hash.of_string (Sha1.to_bin (Sha1.string info_str)) in
  let pieces_hash = Array.map (split pieces Bt_hash.length) ~f:Bt_hash.of_string in 
  let num_pieces = Array.length pieces_hash in
  let files_info = (
    match B.dict_get info_dict_bc "length" with
    | Some length_bc ->
      let length = B.as_int_exn length_bc in
      let name_bc = B.dict_get_exn info_dict_bc "name" in
      let name = B.as_string_exn name_bc in 
      [name, length]
    | None -> 
      let files_bc = B.dict_get_exn info_dict_bc "files" in
      let files = B.as_list_exn files_bc in 
      let f (file_info_bc: B.t) : string * int =
        let name_bc = B.dict_get_exn file_info_bc "path" in
        let name_list = B.as_list_exn name_bc  in 
        let names = List.map name_list ~f:B.as_string_exn in
        let length_bc = B.dict_get_exn file_info_bc "length" in
        let length = B.as_int_exn length_bc in
        (Filename.of_parts names), length
      in
      List.map files f) 
  in 
  let num_files = List.length files_info in 
  let total_length = List.fold files_info ~init:0 ~f:(fun acc (_,l) -> l + acc)  in

  if not (num_pieces = (total_length + piece_length - 1) / piece_length) then
    (* TODO replace with proper exception *)
    assert false;

  info "torrent: %s" torrent_name;
  info "torrent: %d files" num_files;
  info "torrent: %d pieces" num_pieces;
  info "torrent: piece length = %d" piece_length;
  info "torrent: announce %s" announce;

 let al : string list = List.concat announce_list in  
  List.iter al ~f:(info "torrent: announce %s");

  { announce; info_hash; piece_length; pieces_hash; announce_list; 
    files_info; torrent_name; total_length; num_pieces; num_files }


let from_file file_name =
  In_channel.with_file file_name ~f:(do_file file_name)
