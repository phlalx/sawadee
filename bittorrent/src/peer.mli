(** Worker thread to communicate with a remote peer. 

    This module implements a "worker thread" that communicates with a remote 
    peer, and controlled by a "master" [Pwp.t].

    A [Peer.t] communicates with [Pwp.t] in two ways.
    - non blocking function calls (e.g. requesting a piece, or sending a
    bitfield.  
    - by notifying it with events on a pipe.

    All [Peer.t] in a swarm and their controling [Pwp.t] eventually share a
    [Network_file.t]. This let them answer block queries from
    remote peers with no interaction with [Pwp.t]. *)

open Core
open Async
open Log.Global

type t

val create : Peer_id.t -> Peer_comm.t -> Network_file.t option 
  -> (Pevent.t * t) Pipe.Writer.t -> dht:bool -> extension:bool -> t

(** starts the message loop. should be done before any other operation.  *)
val start : t -> unit Deferred.t

val close : t -> unit Deferred.t 

val set_nf : t -> Network_file.t -> unit

val id : t -> Peer_id.t

val to_string : t -> string

val has_piece : t -> int -> bool

val status : t -> Status.peer_status

val request_meta : t -> unit

val send_have : t -> int -> unit



