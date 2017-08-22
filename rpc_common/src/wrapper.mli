open Core
open Async

val add_torrent : string -> int -> Bt_hash.t Deferred.t

val add_magnet : string -> int -> Bt_hash.t Deferred.t

val terminate : int -> unit Deferred.t

val status : Bt_hash.t -> int -> Status.t option Deferred.t

val seed : (string * int) -> int -> Bt_hash.t Deferred.Or_error.t