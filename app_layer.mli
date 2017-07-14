(** Application layer. *)

open Core
open Async

type t

val create: File.t -> peer_id:string -> t

val start: t -> unit

val add_peer: t -> Socket.Address.Inet.t -> unit


