(** Http client to query the tracker. 

    TODO: 
    - implement all types of querty
    - handle errors *)
open Async

(** Initialize client with data used in subsequent queries *)
val init : announce:string -> info_sha1:string -> length:int -> unit

(* Initial query, retrieve list of peers. *)
val query: unit -> Socket.Address.Inet.t list Deferred.t
