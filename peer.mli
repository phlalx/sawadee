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
  have : Bitset.t;
  mutable pending : Int.Set.t;
  mutable time_since_last_reception : int;
  mutable time_since_last_send : int;
  mutable idle : bool;
}


exception Handshake_error

val create: Socket.Address.Inet.t -> piece_num:int -> (t,exn) result Deferred.t 

(** [handshake x info_hash peer_id] tries to handshake with peer x with
    info_hash. Set peer id as a side effect. Return error or nil. *) 
val handshake: t -> string -> string -> (unit, exn) result Deferred.t

val send_message : t -> Message.t -> unit

val get_message : t -> Message.t Reader.Read_result.t Deferred.t

(* IP address/port *)
val to_string : t -> string

val has_piece : t -> int -> bool

val is_interested : t -> bool

val incr_time : t -> unit 

val is_idle : t -> bool

val pending_to_string : t -> string