(* http://www.bittorrent.org/beps/bep_0003.html - see metainfo files *)

open Core

module Be = Bencode_ext

(* name and length of each individual files *)
type file_info = string * int
[@@deriving bin_io, sexp]

let filename_of_bencode b = 
  Be.as_list_exn b |> List.map ~f:Be.as_string_exn |> Filename.of_parts 

let filename_to_bencode f = 
  match Filename.parts f with
  | [] -> assert false
  | x :: l -> 
    assert (x = ".");
    Be.List (List.map ~f:(fun s -> Be.String s) l)

type info = {
  name : string;
  piece_length : int;
  pieces_hash : Bt_hash.t Array.t sexp_opaque;
  files_info : file_info list; 
  total_length : int;
  num_pieces : int;
  num_files : int;
  priv : int option;
} [@@deriving bin_io, sexp]

type t = {
  info_hash : Bt_hash.t;
  announce : Uri.t;
  announce_list : Uri.t list list;
  tinfo : info;
} [@@deriving sexp]

let info_to_bencode i = 
  let pieces = Array.map i.pieces_hash ~f:Bt_hash.to_string |> 
               Array.to_list |> String.concat in

  let com =
    [
     ("name", Be.String i.name ) ;

    ("piece length", Be.Integer i.piece_length);
     ("pieces", Be.String pieces); 
     ] 
  in
  let com = 
    match i.priv with
    | None -> com
    | Some i -> com @ [ ("private"), Be.Integer i ]
  in
  match i.files_info with
  | [] -> assert false
  | [name, len] -> 
    assert (name = i.name);
    Be.Dict (("length", Be.Integer len) :: com)
  | fi -> 
    let f (n,l) =
      let a1 = "length", Be.Integer l in
      let a2 = "path", (filename_to_bencode n) in
      Be.Dict [ a1; a2 ] 
    in
    let l = List.map fi ~f in
    let files = Be.List l in  
    Be.Dict (("files", files) :: com)

let info_to_string i : string = 
  let b = info_to_bencode i in
  Be.encode_to_string b

let info_to_string_hum t = Sexp.to_string (sexp_of_info t)

let info_of_bencode b : info = 
  let name = Be.dict_get_string_exn b "name" in
  let pieces = Be.dict_get_exn b "pieces" in
  let piece_length = Be.dict_get_int_exn b "piece length" in
  let priv = Option.map (Be.dict_get b "private") ~f:Be.as_int_exn in

  let pieces_hash = Be.split pieces Bt_hash.length in
  let pieces_hash = List.map pieces_hash ~f:Bt_hash.of_bencode in
  let pieces_hash = Array.of_list pieces_hash in
  let num_pieces = Array.length pieces_hash in

  let files_info = 
    match Be.dict_get b "length" with
    | Some length_bc ->
      let length = Be.as_int_exn length_bc in
      [name, length]
    | None -> 
      let files = Be.dict_get_list_exn b "files" in
      let f (file_info_bc: Be.t) : string * int =
        let names = Be.dict_get_exn file_info_bc "path" |> filename_of_bencode in
        let length = Be.dict_get_int_exn file_info_bc "length" in
        names, length
      in
      List.map files f
  in 
  let num_files = List.length files_info in 
  let total_length = 
    List.fold files_info ~init:0 ~f:(fun acc (_,l) -> l + acc)  in
  (* TODO proper exception *)
  assert (num_pieces = (total_length + piece_length - 1) / piece_length); 
  { name; piece_length; pieces_hash; files_info; total_length; num_pieces; 
    num_files; priv }

let info_of_string s = `String s |> Be.decode |> info_of_bencode

let bencode_to_uri x = x |> Be.as_string_exn |> Uri.of_string

let of_bencode bc =
  let announce_bc = Be.dict_get_exn bc "announce" in
  let announce = bencode_to_uri announce_bc in
  let announce_list : Uri.t list list =
    match Be.dict_get bc "announce-list" with  
    | None -> []
    | Some al -> 
      let al : Be.t list = Be.as_list_exn al in
      let f (x:Be.t) : Uri.t list = 
        Be.as_list_exn x |> List.map ~f:bencode_to_uri
      in
      List.map al ~f
  in
  let info_dict_bc = Be.dict_get_exn bc "info" in 
  let info_str = Be.encode_to_string info_dict_bc in 
  let info_hash = Bt_hash.sha1_of_string info_str in
  let tinfo = info_of_bencode info_dict_bc in
  { announce; announce_list; info_hash; tinfo }

let of_string s = `String s |> Be.decode |> of_bencode

let info_of_file name ~piece_length = 
  let ic = In_channel.create name in
  let name = Filename.basename name in
  let total_length = Int64.to_int_exn (In_channel.length ic) in
  let num_pieces = (total_length + piece_length - 1) / piece_length in
  let buf = String.create piece_length in
  let f i = 
     (* TODO inchannel or reader? *)
     let p = In_channel.input ic ~buf ~pos:0 ~len:piece_length in
     Bt_hash.sha1_of_string (String.sub buf ~pos:0 ~len:p)
   in

  let pieces_hash = Array.init num_pieces ~f in
  {
    name;
    piece_length;
    pieces_hash;
    files_info = [ (name, total_length )];
    total_length;
    num_pieces;
    num_files = 1;
    priv = None;
  }

let to_string_hum t = Sexp.to_string (sexp_of_t t)






