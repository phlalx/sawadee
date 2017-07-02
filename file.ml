open Core

type t = {
  file : string;
  length : int;
  num_pieces : int;
  pieces_hash : string Array.t;
  pieces_download : bool Array.t;
  sha1 : string;
}

let create length sha1 pieces =  
  let file = String.create length in
  let pieces_hash = List.to_array pieces in
  let num_pieces = Array.length pieces_hash in
  let pieces_download = Array.create num_pieces false in
  { file; length; num_pieces; pieces_hash; pieces_download; sha1 }

