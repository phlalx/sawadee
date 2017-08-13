(** Network file.

  A network file contains:
   - A large array representing the network file, also seen as a sequence of 
     [Piece.t]
   - the status of each [Piece.t] : downloaded or not
   - info relative to persistence (where to save each piece).
   - the meta-info Torrent.info 

  TODO: rename this package? *)

open Core
open Async
open Log.Global

type t

val create : Bt_hash.t -> Torrent.info -> t Deferred.t

val get_piece : t -> int -> Piece.t  

val set_downloaded : t -> int -> unit

val is_valid_piece_index : t -> int -> bool 

val num_pieces : t -> int

val length : t -> int

val has_piece : t -> int -> bool 

val downloaded : t -> Bitfield.t

val has_any_piece : t -> bool

val write_piece : t -> int -> unit

val tinfo : t -> Torrent.info

val close : t -> unit Deferred.t 

