(** Network File to be downloaded by the P2P protocol.

    A [File.t] is an array of [Piece.t]. It maintains the state of each piece
    as well as a bitset of owned pieces. *)

open Core
open Async

type t

(** [create ph ~piece_length total_length bf] creates a [File.t]. 
    - [h] is the info hash
    - [ph] is the array of pieces hashs
    - [bf] is the bitfield of pieces already downloaded. 

    TODO: check why is bitfield need here *)

val create : 
  (Bt_hash.t Array.t) -> 
  piece_length:int -> 
  total_length:int ->
  t

val set_piece_status : t -> int -> 
  [`Requested | `Downloaded | `Not_requested ] -> unit

val get_piece_status : t -> int -> 
  [`Requested | `Downloaded | `Not_requested ]

val length : t -> int

val get_piece : t -> int -> Piece.t

val is_valid_piece_index : t -> int -> bool

(* val deferred_iter_piece : t -> f:(Piece.t -> unit Deferred.t) -> unit Deferred.t *)

val num_pieces : t -> int

(* true iff is downloaded or on disk *)
val has_piece : t -> int -> bool 

val num_owned_pieces : t -> int

val pieces_not_requested : t -> Bitset.t

val pieces_to_string : t -> string

(** This is the [Bitfield.t] decribing the list of pieces we have *)
val bitfield : t -> Bitfield.t

