(** Network File to be downloaded by the P2P protocol.

  A [File.t] is an array of [Piece.t]. It maintains the state of each piece
  as well as a bitset of owned pieces.

  TODO: This is not a good abstraction because we access sometimes [File.t]
  and sometimes [Piece.t] from [Pwp]. It means the client module) needs to ensure 
  that some invariants hold (i.e. state of a piece compatible with bitset). *)

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
  Bitfield.t ->
  t

val length : t -> int

val get_piece : t -> int -> Piece.t

val deferred_iter_piece : t -> f:(Piece.t -> unit Deferred.t) -> unit Deferred.t

val num_pieces : t -> int

val set_owned_piece : t -> int -> unit

val has_piece : t -> int -> bool

val num_owned_pieces : t -> int

val pieces_not_requested : t -> Bitset.t

val pieces_to_string : t -> string

(** This is the [Bitfield.t] decribing the list of pieces we have *)
val bitfield : t -> Bitfield.t

