(** Server that waits for incoming requests from remote peers. 

    An incoming peer goes through the following steps:
    - creation of [Peer_comm.t]
    - handshake with [Peer_comm.wait_handshake]
    - if the requested [Bt_hash.t] is in the [Torrent_table]
    - add to swarm with [Swarm.add_peer_comm] *)

open Core
open Async

val start : port:int -> unit Deferred.t