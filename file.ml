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
}

let num_owned_pieces t = Bitset.card t.owned_pieces

let piece_init pieces_hash piece_length total_len i = 
  let adjusted_piece_length = min (total_len - i * piece_length) piece_length in
  Piece.create i pieces_hash.(i) adjusted_piece_length

let create pieces_hash ~piece_length ~total_length =

  let num_pieces = Array.length pieces_hash in
  (* TODO: move that to a validation function of the torrent file *)
  assert (num_pieces = (total_length + piece_length - 1) / piece_length);

  let owned_pieces = Bitset.empty ~size:num_pieces in

  let f = piece_init pieces_hash piece_length total_length in
  let pieces = Array.init num_pieces ~f  in
  (* Deferred.Array.iteri ~how:`Sequential pieces ~f *)
  Deferred.unit 
  >>| fun () ->
  info "create file (num piece = %d)" num_pieces;
  { len = total_length; 
    num_pieces; 
    owned_pieces; 
    piece_length;
    pieces;
  } 

let bitfield t = Bitset.to_bitfield t.owned_pieces

let num_pieces t = t.num_pieces 

let get_piece t i = t.pieces.(i)

let set_owned_piece t i = Bitset.insert t.owned_pieces i

let pieces_not_requested t = 
  let f i = (Piece.get_status t.pieces.(i)) = `Not_requested in
  Bitset.create ~size:t.num_pieces ~f

let has_piece t i = Bitset.belongs t.owned_pieces i

let length t = t.len 





