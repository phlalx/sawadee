open Core

type t = {
  file : string;
  length : int;
  num_pieces : int;
  pieces_hash : string Array.t;
  pieces_download : bool Array.t;
  sha1 : string;
}

val create : int -> string -> string list -> t
