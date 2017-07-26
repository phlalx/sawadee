(** Network File to be downloaded by the P2P protocol.

    A [File.t] is an array of [Piece.t]. It maintains the state of each piece
    as well as a bitset of owned pieces. *)

open Core
open Async

type t

(** first parameter is array of pieces hash *) 
val create : (Bt_hash.t Array.t) -> piece_length:int -> total_length:int -> t

val set_piece_status : t -> int -> [`Requested | `Downloaded | `Not_requested ] 
  -> unit

val get_piece_status : t -> int -> [`Requested | `Downloaded | `Not_requested ]

val length : t -> int

val get_piece : t -> int -> Piece.t

val is_valid_piece_index : t -> int -> bool

val num_pieces : t -> int

val has_piece : t -> int -> bool 

val num_downloaded_pieces : t -> int

val pieces_not_requested : t -> Bitset.t

(** string of the list of downloaded pieces *)
val pieces_to_string : t -> string

(** This is the [Bitfield.t] decribing the set of pieces we have *)
val bitfield : t -> Bitfield.t

(** returs percentage of file on disk *)
val percent : t -> int

