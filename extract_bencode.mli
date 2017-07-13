(** Extract relevant information from bencode binary.

    We get bencode from the torrent file passed as arg in the command-line,
    and from the http-server (the {i tracker}).

    @raise Wrong_format if binary doesn't have the expected structure. *)


open Core
open Async

type file_info = {
  name : string;
  length : int;
}

type torrent_info = {
  info_hash : string;
  announce : string;
  piece_length : int;
  pieces_hash : string Array.t;
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
