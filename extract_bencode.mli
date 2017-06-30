open Core

type torrent_info = {
  name : string;
  info_sha1 : string;
  announce : string;
  pieces : string;
  length : int;
}

type tracker_reply = {
  complete : int;
  incomplete : int;
  interval : int;
  peers : string
}

exception Wrong_Format

val from_torrent : In_channel.t -> torrent_info

val from_tracker_reply : string -> tracker_reply
