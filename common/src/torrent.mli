(** Information directly obtained from a torrent.

    @raise Wrong_format if binary doesn't have the expected structure. *)

open Core

type file_info = string * int (* name and length of each individual files *)
 [@@deriving bin_io, sexp]

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

val info_of_file : string -> piece_length:int -> info

val info_of_string : string -> info

val info_to_string : info -> string

val info_to_string_hum : info -> string

type t = {
  info_hash : Bt_hash.t; 
  announce : Uri.t;
  announce_list : Uri.t list list;
  tinfo : info; 
}

(* of bencoded string *)
val of_string : string -> t 

val to_string_hum : t -> string