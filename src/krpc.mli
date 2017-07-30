(** This DHT node. 

    Maintains routing table... *)

open Core
open Async
open Log.Global

val try_add : Socket.Address.Inet.t -> unit Deferred.Or_error.t

(* read and ping nodes *)
val read_routing_table : unit -> unit Deferred.t

val write_routing_table : unit -> unit

val lookup : Bt_hash.t -> unit Deferred.Or_error.t