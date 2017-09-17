(** RPC calls to remote node. *)

open Core
open Async

val ping : Node_id.t -> Addr.t -> Node_id.t Deferred.Or_error.t 

val get_peers : Node_id.t -> Addr.t -> Bt_hash.t ->
 [`Values of Addr.t list | `Nodes of Node_info.t list] Deferred.Or_error.t  
 (* TODO add token in result  *)

val find_node : Node_id.t -> Addr.t -> Node_id.t -> (Node_id.t * Node_info.t list) Deferred.Or_error.t 

val announce : Node_id.t -> Addr.t -> Bt_hash.t -> Krpc_packet.port -> 
  Krpc_packet.token -> Node_id.t Deferred.Or_error.t
