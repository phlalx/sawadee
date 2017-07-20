open Core
open Async

type t

val create : 
  string -> int -> 
  (string * int) list ->
  int (* num_pieces *) ->
  int (* piece_length *) ->
  t Deferred.t

val close_all_files : t -> unit Deferred.t

val read_bitfield : t -> Bitfield.t Deferred.t

val write_bitfield : t -> Bitfield.t -> unit Deferred.t

val write_piece : t -> Piece.t -> unit

val read_piece : t -> Piece.t -> unit Deferred.t



