(** Worker thread to communicate with a remote peer. 

    This module implements a "worker thread" that communicates with a remote 
    peer, and controlled by a "master" [Swarm.t].

    A [Peer.t] communicates with [Swarm.t] in two ways.
    - non blocking function calls (e.g. requesting a piece, or sending a
    bitfield.  
    - by notifying it with events on a pipe.

    All [Peer.t] in a swarm and their controling [Swarm.t] eventually share a
    [Network_file.t]. This let them answer block queries from
    remote peers with no interaction with [Swarm.t]. *)

open Core
open Async

type t

val id : t -> Peer_id.t 

val create : Bt_hash.t -> Peer_id.t -> Peer_comm.t -> Network_file.t option 
  -> (Pevent.t * t) Pipe.Writer.t -> dht:bool -> extension:bool -> t

(** starts the message loop. should be done before any other operation.  *)
val start : t -> unit Deferred.t

val close : t -> unit Deferred.t 

val set_nf : t -> Network_file.t -> unit

val to_string : t -> string

val status : t -> Status.peer_status

val request_meta : t -> unit

(** advertise new piece and udpate interested status *)
val notify : t -> int -> unit
