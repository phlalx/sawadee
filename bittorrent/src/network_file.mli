open Core
open Async
open Log.Global

type t

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

val create : Torrent.info -> string -> t Deferred.t

val close : t -> unit Deferred.t 





