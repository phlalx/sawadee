(** File to be downloaded by the P2P protocol. *)
open Core
open Async

type t

val create : len:int -> hash:string -> pieces_hash:(string Array.t) 
  -> name:string ->  piece_length:int -> t Deferred.t

val write_to_disk : t -> unit 

val get_piece : t -> int -> Piece.t

val num_pieces : t -> int

val set_piece_have : t -> int -> unit

val num_piece_have : t -> int

val hash : t -> string

val bitset : t -> string
