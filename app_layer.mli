(** Application layer. *)

open Core
open Async

type t

val create: File.t -> peer_id:string -> t

val start: t -> Socket.Address.Inet.t list -> unit Deferred.t


