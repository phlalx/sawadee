(** Information directly obtained from a torrent.

    @raise Wrong_format if binary doesn't have the expected structure. *)

open Core

type file_info = string * int (* name and length of each individual files *)

type info = {
  piece_length : int;
  pieces_hash : Bt_hash.t Array.t;
  files_info : file_info list; 
  total_length : int;
  num_pieces : int;
  num_files : int;
}

val info_of_string : string -> info

val info_to_string : info -> string

type t = {
  info_hash : Bt_hash.t; 
  announce : Uri.t;
  announce_list : Uri.t list list;
  tinfo : info; 
}

val of_string : string -> t

(* val to_string : t -> info -> string *)