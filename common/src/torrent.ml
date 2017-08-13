(* http://www.bittorrent.org/beps/bep_0003.html - see metainfo files *)

open Core
open Async
open Log.Global

module B = Bencode_ext

(* name and length of each individual files *)
type file_info = string * int
[@@deriving bin_io, sexp]

let filename_of_bencode b = 
  B.as_list_exn b |> List.map ~f:B.as_string_exn |> Filename.of_parts 

let filename_to_bencode f = 
  match Filename.parts f with
  | [] -> assert false
  | x :: l -> 
    assert (x = ".");
    B.List (List.map ~f:(fun s -> B.String s) l)

type info = {
  name : string;
  piece_length : int;
  pieces_hash : Bt_hash.t Array.t sexp_opaque;
  files_info : file_info list; 
  total_length : int;
  num_pieces : int;
  num_files : int;
} [@@deriving bin_io, sexp]

type t = {
  info_hash : Bt_hash.t;
  announce : Uri.t;
  announce_list : Uri.t list list;
  tinfo : info;
}

let info_to_bencode i = 
  let pieces = Array.map i.pieces_hash ~f:Bt_hash.to_string |> 
               Array.to_list |> String.concat in
  let com =
    [("piece length", B.Integer i.piece_length);
     ("pieces", B.String pieces); 
     ("name", B.String i.name ) ]
  in
  match i.files_info with
  | [] -> assert false
  | [name, len] -> 
    assert (name = i.name);
    B.Dict (("length", B.Integer len) :: com)
  | fi -> 
    let f (n,l) =
      let a1 = "path", (filename_to_bencode n) in
      let a2 = "length", B.Integer l in
      B.Dict [ a1; a2 ] 
    in
    let l = List.map fi ~f in
    let files = B.List l in  
    B.Dict (("files", files) :: com)

let info_to_string i : string = 
  let b = info_to_bencode i in
  debug !"Torrent: info_to_bencode %{B.pretty_print}" b;
  B.encode_to_string b

let info_to_string_hum i : string =
  let fi =
    match i.files_info with
    | [] -> ""
    | _ :: [] -> "" 
    | fis ->
      let s = List.map fis ~f:(fun (n,i) -> sprintf "%s %d" n i)
      in (String.concat ~sep:"\n" s) ^ "\n"
  in

  sprintf "%s\npiece_length: %dkB\ntotal_length: %dkB\nnum_pieces: %d\n%s" 
    i.name (i.piece_length / 1000) (i.total_length / 1000) i.num_pieces fi

let info_of_bencode b : info = 
  debug !"Torrent: info_of_bencode %{B.pretty_print}" b;
  let name = B.dict_get_string_exn b "name" in
  let pieces = B.dict_get_exn b "pieces" in
  let piece_length = B.dict_get_int_exn b "piece length" in

  let pieces_hash = B.split pieces Bt_hash.length in
  let pieces_hash = List.map pieces_hash ~f:Bt_hash.of_bencode in
  let pieces_hash = Array.of_list pieces_hash in
  let num_pieces = Array.length pieces_hash in

  let files_info = 
    match B.dict_get b "length" with
    | Some length_bc ->
      let length = B.as_int_exn length_bc in
      [name, length]
    | None -> 
      let files = B.dict_get_list_exn b "files" in
      let f (file_info_bc: B.t) : string * int =
        let names = B.dict_get_exn file_info_bc "path" |> filename_of_bencode in
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
  { name; piece_length; pieces_hash; files_info; total_length; num_pieces; 
    num_files }

let info_of_string s = `String s |> B.decode |> info_of_bencode

let bencode_to_uri x = x |> B.as_string_exn |> Uri.of_string

let of_bencode bc =
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


let of_string s = `String s |> B.decode |> of_bencode








