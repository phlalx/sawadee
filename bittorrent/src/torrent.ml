open Core
open Async
open Log.Global

module B = Bencode_ext
module G = Global

type file_info = string * int (* name and length of each individual files *)

type info = {
  piece_length : int;
  pieces_hash : Bt_hash.t Array.t;
  files_info : file_info list; 
  total_length : int;
  num_pieces : int;
  num_files : int;
}

type t = {
  info_hash : Bt_hash.t;
  announce : Uri.t;
  announce_list : Uri.t list list;
  tinfo : info;
}

let info_of_bencode (info_dict_bc : B.t) : info = 
  let pieces = B.dict_get_exn info_dict_bc "pieces" in
  let piece_length = B.dict_get_int_exn info_dict_bc "piece length" in

  let pieces_hash = B.split pieces Bt_hash.length in
  let pieces_hash = List.map pieces_hash ~f:Bt_hash.of_bencode in
  let pieces_hash = Array.of_list pieces_hash in
  let num_pieces = Array.length pieces_hash in

  let files_info = 
    match B.dict_get info_dict_bc "length" with
    | Some length_bc ->
      let length = B.as_int_exn length_bc in
      let name = B.dict_get_string_exn info_dict_bc "name" in [name, length]
    | None -> 
      let files = B.dict_get_list_exn info_dict_bc "files" in
      let f (file_info_bc: B.t) : string * int =
        let names = 
          B.dict_get_list_exn file_info_bc "path"  
          |> List.map ~f:B.as_string_exn
          |> Filename.of_parts 
        in
        let length = B.dict_get_int_exn file_info_bc "length" in
        names, length
      in
      List.map files f
  in 
  let num_files = List.length files_info in 
  let total_length = 
    List.fold files_info ~init:0 ~f:(fun acc (_,l) -> l + acc)  in
  (* TODO proper exception *)
  assert (num_pieces = (total_length + piece_length - 1) / piece_length); 
  info "Torrent: %d files" num_files;
  info "Torrent: %d pieces" num_pieces;
  info "Torrent: piece length = %d" piece_length;
  { piece_length; pieces_hash; files_info; total_length; num_pieces; num_files }

let bencode_to_uri x = x |> B.as_string_exn |> Uri.of_string

let decode bc =
  debug !"Torrent: bc = %{B.pretty_print}" bc;
  let announce_bc = B.dict_get_exn bc "announce" in
  let announce = bencode_to_uri announce_bc in
  let announce_list : Uri.t list list =
    match B.dict_get bc "announce-list" with  
    | None -> []
    | Some al -> 
      let al : B.t list = B.as_list_exn al in
      let f (x:B.t) : Uri.t list = 
        B.as_list_exn x |> List.map ~f:bencode_to_uri
      in
      List.map al ~f
  in
  let info_dict_bc = B.dict_get_exn bc "info" in 
  let info_str = B.encode_to_string info_dict_bc in 
  let info_hash = Sha1.string info_str |> Sha1.to_bin |> Bt_hash.of_string in
  let tinfo = info_of_bencode info_dict_bc in
  debug !"Torrent: announce %{Uri}" announce;
  List.concat announce_list |>  List.iter ~f:(debug !"Torrent: announce %{Uri}"); 
  { announce; announce_list; info_hash; tinfo }

let decode_channel c = `Channel c |> B.decode |> decode 

let from_file file = In_channel.with_file file ~f:decode_channel

let from_string s = `String s |> B.decode |> decode
