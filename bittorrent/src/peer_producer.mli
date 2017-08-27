(** Produce peers for [Swarm.t] to consume.

    Peers are found from two sources:
    - tracker if an [Uri.t] is given
    - DHT 

    An [Mvar] is used as a gate to stop the producer from producing peers.
    If the [Mvar] is full, the producer queries the DHT and the tracker at 
    regular interval. 
   
    The peers correctly handhshaked before they were pushed to the pipe.

    TODO: we have a lot of false positive peers (already added) that get 
    rejected by the swarm based on their peer_id. Maybe we could get rid
    of them based on their IP before attempting to push them). *)
open Core
open Async

type t

val create : unit Mvar.Read_only.t -> 
  (Peer_comm.t * Peer_comm.handshake_info) Pipe.Writer.t -> Bt_hash.t 
  -> Uri.t option -> t

val start : t -> unit
