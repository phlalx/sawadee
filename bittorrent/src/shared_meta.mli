(** State shared by all peers.

  This module contains all the state shared by the [Peers.t]s and [Swarm.t]. 
  It is created once meta-info is known.

  It contains:
   - the network file [Bigstring.t].
   - the downloaded status of each [Piece.t]
   - persistence info (where to save each piece, and the bitfield)
   - Torrent.info 
   - the set of requested pieces. 
   
  TODO: split this module into several parts *) 

open Core
open Async

type t

(** [create s h ti] tries to recover downloaded files and bitfield from 
    a previous session. Creates the files otherwise. If [s = true], all the files
    must be there and a full bitfield is created
    
    TODO: 
      - deal with errors
      - we read all the files initially to put them in memory... it would
        be better to map them memory, or use a cache of some sort *)
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