(** remote DHT node *)

open Core
open Async
open Log.Global

type t 

val create : Socket.Address.Inet.t -> Node_id.t -> t

val get_message : t -> Krpc_packet.t Reader.Read_result.t Deferred.t

val send_message : t -> Krpc_packet.t -> unit

val addr : t -> Socket.Address.Inet.t