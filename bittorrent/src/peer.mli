(** Worker thread to communicate with a remote peer. 

    This module implements a "worker thread" that communicates with a remote 
    peer, and controlled by a "master" [Swarm.t].

    A [Peer.t] communicates with [Swarm.t] in two ways.
    - from swarm to peers, via function call (e.g. [request_meta], [start], 
      [stop]) (TODO: via updating [Shared_meta]) .
    - from peers to swarm, via an event pipe

    All [Peer.t] in a swarm and their controling [Swarm.t] eventually share a
    [Shared_meta.t] datastructure. *)

open Core
open Async

type t

val id : t -> Peer_id.t 

val create : Bt_hash.t -> Peer_id.t -> Peer_comm.t -> Shared_meta.t option 
  -> (Pevent.t * t) Pipe.Writer.t -> dht:bool -> extension:bool -> t

(** starts the message loop. should be done before any other operation.  *)
val start : t -> unit Deferred.t

val close : t -> unit Deferred.t 

val set_shared_meta : t -> Shared_meta.t -> unit

val to_string : t -> string

val status : t -> Status.peer_status

val request_meta_info : t -> unit

(** advertise new piece and udpate interested status *)
val notify : t -> int -> unit
