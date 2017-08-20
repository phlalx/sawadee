(** remote DHT node *)

open Core
open Async
open Log.Global

module Kp = Krpc_packet

type t 

(* id of querying node *)
val connect : Node_id.t -> Addr.t -> t

val ping : t -> Node_id.t Deferred.Or_error.t 

val get_peers : t -> Bt_hash.t ->
 [`Values of Addr.t list | `Nodes of Node_info.t list] Deferred.Or_error.t  
 (* TODO add token in result  *)

val find_node : t -> Node_id.t -> (Node_id.t * Node_info.t list) Deferred.Or_error.t 

val announce : t -> Bt_hash.t -> Kp.port -> Kp.token ->  (* TODO use label instead *)
 Node_id.t Deferred.Or_error.t

val close : t -> unit