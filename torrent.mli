(** Extract relevant information from torrent (meta-info file)

    @raise Wrong_format if binary doesn't have the expected structure. *)

open Core

exception Wrong_Format

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

val from_chan : In_channel.t -> t
