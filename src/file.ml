open Core
open Async
open Log.Global
module G = Global

type t = {
  len : int; (** total length of network file *)
  piece_length : int; (** piece length of all pieces except possibly the last one *)
  num_pieces : int; (** number of pieces to be downloaded *)
  pieces : Piece.t Array.t;
  downloaded_pieces : Bitset.t;
  pieces_status : [ `Requested | `Downloaded | `Not_requested ] Array.t;
}

let piece_init pieces_hash piece_length total_len downloaded_pieces i = 
  let adjusted_piece_length = min (total_len - i * piece_length) piece_length in
  let p = Piece.create i pieces_hash.(i) adjusted_piece_length in
  p

let create pieces_hash ~piece_length ~total_length =

  let num_pieces = Array.length pieces_hash in

  let downloaded_pieces = Bitset.empty ~size:num_pieces in
  let f = piece_init pieces_hash piece_length total_length downloaded_pieces in
  let pieces = Array.init num_pieces ~f  in
  let pieces_status = Array.create num_pieces `Not_requested in

  info "create file (num piece = %d)" num_pieces;
  { len = total_length; 
    num_pieces; 
    downloaded_pieces; 
    piece_length;
    pieces;
    pieces_status;
  } 

let get_piece t i = t.pieces.(i)

let is_valid_piece_index t i = i >=0 && i < t.num_pieces

let num_pieces t = t.num_pieces 

let length t = t.len 

let has_piece t i = Bitset.belongs t.downloaded_pieces i

let num_downloaded_pieces t = Bitset.card t.downloaded_pieces

let set_piece_status t i s = 
  t.pieces_status.(i) <- s;
  match s with
  | `Requested | `Not_requested -> 
    assert (not (s = `Downloaded))
  | `Downloaded -> Bitset.insert t.downloaded_pieces i

let get_piece_status t i = 
  let s = t.pieces_status.(i) in
  (match s with 
   | `Requested | `Not_requested -> assert(not (has_piece t i)) 
   | `Downloaded -> assert((has_piece t i))
  ); s

let pieces_not_requested t = 
  let f i = t.pieces_status.(i) = `Not_requested in
  Bitset.create ~size:t.num_pieces ~f

let pieces_to_string t = Bitset.to_string t.downloaded_pieces

let bitfield t = Bitset.to_bitfield t.downloaded_pieces

let percent t = (100 * (num_downloaded_pieces t)) / t.num_pieces











