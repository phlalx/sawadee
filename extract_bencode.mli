(** Extract relevant information from bencode binary.

    We get bencode from the torrent file passed as arg in the command-line,
    and from the http-server (the {i tracker}).

    @raise Wrong_format if binary doesn't have the expected structure. *)


open Core
open Async

type torrent_info = {
  name : string; (** name of file to be downloaded *)
  info_sha1 : string; (** 20-bytes sha of info section *)
  announce : string; (** address of tracker *)
  piece_length : int; (** number of pieces to download *)
  pieces : string list; (* sha1 of each pieces *)
  length : int; (* length of file *)
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
