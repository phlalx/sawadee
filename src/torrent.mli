(** Information directly obtained from a torrent.

    @raise Wrong_format if binary doesn't have the expected structure. *)

open Core

type t = {
  info_hash : Bt_hash.t;
  announce : string;
  announce_list : string list list;
  piece_length : int;
  pieces_hash : Bt_hash.t Array.t;
  files_info : (string * int) list; (* name and length of each individual files *)
  torrent_name : string;
  total_length : int;
  num_pieces : int;
  num_files : int;
}

(** [from_file f] try to open file [f] and decode it

  Raises: 
    - Bencode_utils.Bencode_error
    - Sys_error
    - Failure  *)
val from_file : string -> t
