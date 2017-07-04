(** Http client to query the tracker. 

    TODO: 
    - implement all types of query *)

open Core
open Async

(** Initialize client with data used in subsequent queries *)
val init : announce:string -> info_sha1:string -> length:int -> unit           

(* Initial query, retrieve list of peers. *)
val query: unit -> (Async.Socket.Address.Inet.t list, exn) result Deferred.t
