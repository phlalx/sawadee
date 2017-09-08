(** Network file.

  A network file contains:
   - a sequence of [Piece.t]
   - the state of each [Piece.t]
   - info relative to persistence (where to save each piece). *)
open Core
open Async
open Log.Global

type t

val create : Bt_hash.t -> Torrent.info -> t Deferred.t

val get_piece : t -> int -> Piece.t  

val set_piece_status : t -> int -> [`Requested | `Not_requested | `Downloaded]
  -> unit  

val is_valid_piece_index : t -> int -> bool 

val num_pieces : t -> int

val length : t -> int

val has_piece : t -> int -> bool 

val downloaded : t -> Bitfield.t

val num_downloaded_pieces : t -> int

val write_piece : t -> int -> unit

val not_requested : t -> int list 

val close : t -> unit Deferred.t 

