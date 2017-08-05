open Core
open Async

type handler

type status = {
  num_peers : int
}

(* fail on error *)
val create: 
  server_port:(int option) ->
  verbose:(int option) ->
  torrent_path:string ->
  download_path:string -> 
  unit Deferred.t

val add_torrent : string -> handler

val add_magnet : string -> handler

val torrent_list : unit -> handler list

val terminate : unit -> unit Deferred.t

val handler_to_string : handler -> string

val handler_of_string : string -> handler Option.t

val status : handler -> status Option.t

val parse_uri : string -> 
  [ `Magnet of string | `File of string | `Invalid_magnet | `Other ]
