open Async

val init : announce:string -> info_sha1:string -> length:int -> unit

val query: unit -> unit Deferred.t
