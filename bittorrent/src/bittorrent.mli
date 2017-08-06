open Core
open Async

(* fail on error *)
val create: 
  server_port:(int option) ->
  verbose:(int option) ->
  torrent_path:string ->
  download_path:string -> 
  unit Deferred.t

val add_torrent : string -> Bt_hash.t

val add_magnet : string -> Bt_hash.t

val torrent_list : unit -> Bt_hash.t list

val terminate : unit -> unit Deferred.t

val status : Bt_hash.t -> Status.t Option.t

