(** Network file.

  A network file contains:
   - A large array representing the network file, also seen as a sequence of 
     [Piece.t]
   - the status of each [Piece.t] : downloaded or not
   - info relative to persistence (where to save each piece).
   - the meta-info Torrent.info 

  TODO: rename this package?
  It contains all the stuff that is shared between every peers once
  we know the meta-info. More than the network file...
   *)

open Core
open Async

type t

val create : seeder:bool -> Bt_hash.t -> Torrent.info -> t Deferred.t

val seeder : t -> bool

val meta_length : t -> int

val tinfo_bin : t -> string

val add_requested : t -> int -> unit

val remove_requested : t -> int -> unit

val requested : t -> int list 

val get_piece : t -> int -> Piece.t  

val set_downloaded : t -> int -> unit

val is_valid_piece_index : t -> int -> bool 

val num_pieces : t -> int

val length : t -> int

val is_downloaded : t -> int -> bool 

val is_requested : t -> int -> bool 

val downloaded : t -> Bitfield.t

val has_any_piece : t -> bool

val write_piece : t -> int -> unit

val tinfo : t -> Torrent.info

val close : t -> unit Deferred.t 

