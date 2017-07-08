(** P2P layer of the protocol. Provides functions for communicating with 
    peer. *)

open Core
open Async

type t = {
  mutable choked : bool; (** true if peer doesn't serve requests (initially true) *)
  mutable interested : bool; (** is peer interested in pieces? (initially false() *)
  peer : Socket.Address.Inet.t;
  mutable id : string;
  reader : Reader.t;
  writer : Writer.t;
  have : Bitset.t
 }

exception Handshake_error

val create: Socket.Address.Inet.t -> piece_num:int -> (t,exn) result Deferred.t 

(** [handshake x info_sha1 peer_id] tries to handshake with peer x with
    info_sha1. Set peer id as a side effect. Return error or nil. *) 
val handshake: t -> string -> string ->(unit, exn) result Deferred.t

(* TODO use result type for error handling *)
val send_message : t -> Message.t -> unit Deferred.t

val get_message : t -> Message.t Deferred.t 

(* readable id *)
val to_string : t -> string
