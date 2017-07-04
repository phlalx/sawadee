open Core

type t = {
  file : string;
  len : int;
  num_pieces : int;
  pieces_hash : string Array.t;
  pieces_downloaded : bool Array.t;
  sha : string;
}

let create ~len ~sha ~pieces =
  let file = String.create len in
  let pieces_hash = List.to_array pieces in
  let num_pieces = Array.length pieces_hash in
  let pieces_downloaded = Array.create num_pieces false in
  { file; len; num_pieces; pieces_hash; pieces_downloaded; sha }
