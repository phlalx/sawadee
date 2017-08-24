(** remote DHT node *)

open Core
open Async

module Kp = Krpc_packet

val ping : Node_id.t -> Addr.t -> Node_id.t Deferred.Or_error.t 

val get_peers : Node_id.t -> Addr.t -> Bt_hash.t ->
 [`Values of Addr.t list | `Nodes of Node_info.t list] Deferred.Or_error.t  
 (* TODO add token in result  *)

val find_node : Node_id.t -> Addr.t -> Node_id.t -> (Node_id.t * Node_info.t list) Deferred.Or_error.t 

val announce : Node_id.t -> Addr.t -> Bt_hash.t -> Kp.port -> Kp.token ->  (* TODO use label instead *)
 Node_id.t Deferred.Or_error.t
