open Core
open Async
open Log.Global

type t = {
  len : int;
  name : string;
  num_pieces : int; (** number of pieces to be downloaded - TODO edundant *)
  pieces : Piece.t Array.t;
  hash : string;  (** hash of the info section of the bittorrent file *)
  mutable pieces_downloaded : int;
}

let create ~len ~hash ~pieces_hash ~name ~piece_length =
  let num_pieces = Array.length pieces_hash in
  assert (num_pieces = (len + piece_length - 1) / piece_length);
  let piece_init i = 
    let adjusted_piece_length = min (len - i * piece_length) piece_length in
    Piece.create i pieces_hash.(i) adjusted_piece_length in  
  let pieces = Array.init num_pieces ~f:piece_init  in
  info "create file (num piece = %d, name = %s)" num_pieces name;
  { len; name; num_pieces; pieces; hash; pieces_downloaded = 0 }

