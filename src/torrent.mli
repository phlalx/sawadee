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

type t = {
  torrent_name : string;
  info_hash : Bt_hash.t;
  announce : Uri.t;
  announce_list : Uri.t list list;
  tinfo : info;
}

(** [from_file f] try to open file [f] and decode it

  Raises: 
    - Sys_error
    - Failure  *)
val from_file : string -> t
