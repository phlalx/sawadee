open Core
open Async
open Log.Global

type t = {
  len : int;
  name : string;
  num_pieces : int; (** number of pieces to be downloaded - TODO edundant *)
  pieces : Piece.t Array.t;
  sha : string;  (** sha1 of the info section of the bittorrent file *)
  mutable pieces_downloaded : int;
}

let create ~len ~sha ~pieces ~name ~piece_length =
  let pieces_hash = List.to_array pieces in
  let num_pieces = Array.length pieces_hash in
  (* last piece may have a different size *)
  (* TODO is this really the case - read the spec *)
  let last_piece_length =
    if len % piece_length = 0 then (
      piece_length
    ) else ( 
      debug "last piece of file has different length";
      len % piece_length
    ) in
  let piece_init i = 
    let adjusted_piece_length  =
      if i = num_pieces - 1 then
        last_piece_length
      else 
        piece_length 
    in
    Piece.create i pieces_hash.(i) adjusted_piece_length in  
  let pieces = Array.init num_pieces ~f:piece_init  in
  info "create file (num piece = %d, name = %s)" num_pieces name;
  { len; name; num_pieces; pieces; sha; pieces_downloaded = 0 }

