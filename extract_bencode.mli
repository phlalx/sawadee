(** Extract relevant information from bencode binary.

    This concerns two different things:
    - The bencode read from the metainfo file (from command-line)
    - The {i tracker} reply.

    This supports both single and multiple file mode.   

    @raise Wrong_format if binary doesn't have the expected structure. *)

open Core
open Async

type file_info = {
  name : string;
  length : int;
}

type torrent_info = {
  info_hash : Bt_hash.t;
  announce : string;
  announce_list : string list list;
  piece_length : int;
  pieces_hash : Bt_hash.t Array.t;
  mode : [`Single_file | `Multiple_file];
  files_info : file_info list
}

type tracker_reply = {
  complete : int;
  incomplete : int;
  interval : int;
  peers : Socket.Address.Inet.t list
}

exception Wrong_Format

val from_torrent : In_channel.t -> torrent_info

val from_tracker_reply : string -> tracker_reply
