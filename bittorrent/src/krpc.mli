(** This DHT node. 

    Maintains routing table... *)

open Core
open Async
open Log.Global

val try_add : Addr.t -> unit Deferred.Or_error.t

val try_add_nis : Node_info.t list  -> unit Deferred.t

(* read and ping nodes *)
val read_routing_table : unit -> unit Deferred.t

val write_routing_table : unit -> unit

val lookup : Bt_hash.t -> Addr.t list Deferred.t

val populate : unit -> unit Deferred.t