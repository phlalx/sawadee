(** remote DHT node *)

open Core
open Async
open Log.Global

type t 

type node_info = Node_id.t * Addr.t 

val connect : Addr.t -> t

val ping : t -> Node_id.t Deferred.Or_error.t 

val get_peers : t -> Bt_hash.t -> [`Values of Addr.t list 
                                  | `Nodes of node_info list] Deferred.Or_error.t  

val close : t -> unit