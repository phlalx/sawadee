open Core

type t = {
  name : string;
  info_sha1 : string;
  announce : string;
  pieces : string
}

exception Wrong_Format

val extract_from_bencode : In_channel.t -> t
