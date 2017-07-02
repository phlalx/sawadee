open Core
open Async

type torrent_info = {
  name : string;
  info_sha1 : string;
  announce : string;
  piece_length : int;
  pieces : string list;
  length : int;
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
