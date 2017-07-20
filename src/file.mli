(** Network File to be downloaded by the P2P protocol.

    All the information to create a file come from the metainfo file. A file
    is divided into [Piece.t] that are created upon creation of the file
    (see [Piece.t] doc). *)

open Core
open Async

type t

(** [create ~len h ph ~name ~pl files] creates a new file. This include creating all 
    the file pieces [Piece.t].

    - [h] is the hash of the info section of the metainfo file.
    - [ph] is an array of hashed of each of individual pieces. 
    - [files] are the file names and their length *)

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

