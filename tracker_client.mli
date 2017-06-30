open Async

val query: announce:string -> info_sha1:string -> unit Deferred.t
