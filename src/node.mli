(** remote DHT node *)

open Core
open Async
open Log.Global

type t 

val create : Socket.Address.Inet.t -> t

val addr : t -> Socket.Address.Inet.t

val set_status : t -> [`Good | `Bad | `Questionable] -> unit 

val id : t -> Node_id.t option

val set_id : t -> Node_id.t -> unit

val ping : t -> Node_id.t Deferred.Or_error.t 