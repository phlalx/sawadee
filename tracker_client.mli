(** Http client to query the tracker. 

    Only the initial query is implemented. *)

open Core
open Async

(** Initialize client with data used in subsequent queries *)
val init : announce:string -> announce_list:string list list -> Bt_hash.t 
  -> len:int -> Peer_id.t -> unit           

(* Initial query, retrieve list of peers. Module must be initialized with
   [init] first  *)
val query: unit -> (Socket.Address.Inet.t list) Option.t Deferred.t
