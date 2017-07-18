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
