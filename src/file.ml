open Core
open Async
open Log.Global
module G = Global

type t = {
  len : int; (** total length of network file *)
  piece_length : int; (** piece length of all pieces except possibly the last one *)
  num_pieces : int; (** number of pieces to be downloaded *)
  pieces : Piece.t Array.t;
  owned_pieces : Bitset.t;
  pieces_status : [ `Requested | `Downloaded | `Not_requested ] Array.t;
}

let num_owned_pieces t = Bitset.card t.owned_pieces

let piece_init pieces_hash piece_length total_len owned_pieces i = 
  let adjusted_piece_length = min (total_len - i * piece_length) piece_length in
  let p = Piece.create i pieces_hash.(i) adjusted_piece_length in
  p

let create pieces_hash ~piece_length ~total_length =

  let num_pieces = Array.length pieces_hash in

  let owned_pieces = Bitset.empty ~size:num_pieces in
  let f = piece_init pieces_hash piece_length total_length owned_pieces in
  let pieces = Array.init num_pieces ~f  in
  let pieces_status = Array.create num_pieces `Not_requested in

  info "create file (num piece = %d)" num_pieces;
  { len = total_length; 
    num_pieces; 
    owned_pieces; 
    piece_length;
    pieces;
    pieces_status;
  } 

let bitfield t = Bitset.to_bitfield t.owned_pieces

let num_pieces t = t.num_pieces 

let get_piece t i = t.pieces.(i)

let pieces_not_requested t = 
  let f i = t.pieces_status.(i) = `Not_requested in
  Bitset.create ~size:t.num_pieces ~f

let has_piece t i = Bitset.belongs t.owned_pieces i

let length t = t.len 

let pieces_to_string t = Bitset.to_string t.owned_pieces

let set_piece_status t i s = 
  t.pieces_status.(i) <- s;
  match s with
  | `Requested | `Not_requested -> 
    assert (not (s = `Downloaded))
  | `Downloaded -> Bitset.insert t.owned_pieces i

let get_piece_status t i = 
  let s = t.pieces_status.(i) in
  (match s with 
   | `Requested | `Not_requested -> assert(not (has_piece t i)) 
   | `Downloaded -> assert((has_piece t i))
  ); s




