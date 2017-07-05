(** P2P layer of the protocol. Provides functions for communicating with 
    peer. *)
    
open Core
open Async

type t

exception Handshake_error

val create: Socket.Address.Inet.t -> (t,exn) result Deferred.t 

(** [handshake x info_sha1] tries to handshake with peer x with
    info_sha1. Return error or peer_id *) 
val handshake: t -> string -> (string, exn) result Deferred.t

(* TODO use result type for error handling *)
val send_message : t -> Message.t -> unit Deferred.t

val get_message : t -> Message.t Deferred.t 

(* readable id *)
val to_string : t -> string
