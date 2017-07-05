(** P2P layer of the protocol. Provides functions for communicating with 
    peer. 

    TODO: add error-handling ([result] as return types)
*)
open Core
open Async

type t

val create: Socket.Address.Inet.t -> (t,exn) result Deferred.t 

val handshake: t -> string -> unit Deferred.t

val send_message : t -> Message.t -> unit Deferred.t

val get_message : t -> Message.t Deferred.t 
