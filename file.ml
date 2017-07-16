open Core
open Async
open Log.Global

type t = {
  len : int;
  piece_length : int;
  num_pieces : int; (** number of pieces to be downloaded *)
  pieces : Piece.t Array.t;
  owned_pieces : Bitset.t;
  files : Pfile.t list;
  bitset : Pfile.t;
}

let num_owned_pieces t = Bitset.card t.owned_pieces

let bitset_name name = "." ^ name

let read = assert false

let create pieces_hash ~torrent_name ~piece_length files =
  let num_pieces = Array.length pieces_hash in
  let len = List.fold files ~init:0 ~f:(fun acc (_,l) -> l + acc) in 
  assert (num_pieces = (len + piece_length - 1) / piece_length);

  let piece_init i = 
    let adjusted_piece_length = min (len - i * piece_length) piece_length in
    Piece.create i pieces_hash.(i) adjusted_piece_length in  

  let pieces = Array.init num_pieces ~f:piece_init  in

  let owned_pieces = Bitset.empty ~size:num_pieces in

  info "create file (num piece = %d, name = %s)" num_pieces torrent_name;

  let t = { len; 
            num_pieces; 
            owned_pieces; 
            piece_length;
            pieces;
            files = [];
            bitset = Pfile.create;
           } 
  in 
  return t

let bitfield t = Bitset.to_bitfield t.owned_pieces

let num_pieces t = t.num_pieces 

let get_piece t i = t.pieces.(i)

let set_owned_piece t i = Bitset.insert t.owned_pieces i

let pieces_not_requested t = 
  let f i = (Piece.get_status t.pieces.(i)) = `Not_requested in
  Bitset.create ~size:t.num_pieces ~f

let has_piece t i = Bitset.belongs t.owned_pieces i

let length t = t.len 

let close = assert false



