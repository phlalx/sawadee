(** 

This module implements a "worker thread" that communicates with a remote peer,
and controlled by a "master" [Pwp.t].

A [Peer.t] communicates with [Pwp.t] in two ways.
 - Using high-level operations such as requesting a piece, or sending a
bitfield.  
 - by notifying it with events.

Serving piece is entirely delegated to the [Peer.t]. This require sharing
 of a [Network_file.t] with [Pwp.t]. *)

open Core
open Async
open Log.Global

type t

val create : Peer_comm.t -> dht:bool -> extension:bool -> t

val id : t -> Peer_id.t

val to_string : t -> string

val has_piece : t -> int -> bool

val bitfield : t -> Bitfield.t

val set_nf : t -> Network_file.t -> unit

(* launches the main message loop. should be done before any other operation *)
val start : t -> unit

type event = 
  | Choke 
  | Unchoke
  | Interested
  | Not_interested 
  | Have
  | Bitfield
  | Bye  (** notify the termination of the remote peer *)

val event_to_string : event -> string

val read_event : t -> event Deferred.t 

val send_bitfield : t -> Bitfield.t -> unit 

val advertise_piece : t -> int -> unit

(** request a piece and directly fill [Network_file.t] *)
val request_piece : t -> int -> unit Deferred.t 

val support_metadata : t -> int Deferred.Option.t

val request_metadata_size : t -> int Deferred.Option.t

val request_metadata : t -> string Deferred.t

val set_am_interested : t -> bool -> unit

val set_am_choking : t -> bool -> unit

val am_interested : t -> bool

val am_choking : t -> bool

val peer_choking : t -> bool

val idle : t -> bool

(** assert a condition dependent on values received by a peer. For instance,
    if peer doesn't behave according to the protocol. raises if false 

    TODO: terminate peer silently instead (monitor?). *) 
val validate : t -> bool -> unit