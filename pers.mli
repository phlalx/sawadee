open Core
open Async

type t

val create : 
  string -> int -> 
  (string * int) list ->
  int (* num_pieces *) ->
  int (* piece_length *) ->
  t Deferred.t

val close : t -> unit Deferred.t

val read_bitfield : t -> Bitfield.t Deferred.t

val write_and_close_bitfield : t -> Bitfield.t -> unit Deferred.t

val write_to_pipe : t -> Piece.t -> unit
