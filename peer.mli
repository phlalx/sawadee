(** P2P layer of the protocol. Provides functions for communicating with 
    peer. 

    TODO: add error-handling. 
    - [Result.t] instead of [Option.t] for handshake and create. 
    - use [Result.t] as a return type for [send_message] and [get_message].
*)
open Core
open Async

type t

(** [create p_addr f] creates TCP connection with peer at [p_addr] in order
    to download file [f]. *)
val create: Socket.Address.Inet.t -> File.t -> t Option.t Deferred.t

val handshake: t -> unit Option.t Deferred.t

val send_message : t -> Message.t -> unit Deferred.t

val get_message : t -> Message.t Deferred.t 