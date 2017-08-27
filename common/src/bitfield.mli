(** Bitfield.

Bitfields are used by peers to signal the pieces already downloaded. They
are serialized as the shortest string that can contain the bit status of all
pieces. For instance, a torrent made of 9 pieces will be stored as 16 bits.
 
 Byte    0               1
 Bit     7 6 5 4 3 2 1 0 7 6  5  4  3  2  1  0 
 Piece   0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 

The bits 0 to 6 of byte 1 must be set to 0. Peers that don't follow these 
conventions must be disconnected.

Note that we may receive [Bitfield] or [Have] messages before we receive the 
torrent meta-data. As a consequence, we need to be able to store and update
bitfields before we can validate them. 

We will denote [size] the number of pieces in a bitfield (e.g. 9 in the previous
example, and [length] the number of bytes in the serialized form (e.g. 2) *)

open Core

type t
[@@deriving sexp, bin_io]

val full : int -> t

(* length of serialized form of [t] *)
val length : t -> int 

(* number of elements to be stored *)
val empty : int -> t

val of_string : string -> t

val to_string : t -> string

val get : t -> int -> bool

val set : t -> int -> bool -> unit

val copy : src:t -> dst:t -> unit 

val card : t -> int

val is_empty : t -> bool

val to_string_hum : t -> int -> string

(* [to_list t n] only considers the [n] first elements of the bf *)
val to_list : t -> int -> int list 

(* [is_subset n t1 t2] only considers the [n] first elements of the bf *)
val is_subset : int -> t -> t -> bool

