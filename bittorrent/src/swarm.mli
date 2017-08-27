(** Controls a swarm of peers.

    This module forms with [Peer] the core of the bittorrent protocol. 
    A [t] is created for each torrent to be downloaded. It maintains a set
    of [Peer.t]. 

    [t] controls when peers join/leave the swarm. Peers can come from three 
    sources.
    - incoming peers from the server 
    - from the DHT
    - from the tracker 

    [t] decides which peers can download piece or serve pieces requests by
    setting their [am_choking], [am_interested] flags. Peers notify [t] with 
    [Pevent.t] sent on a pipe. 

    All peers and [t] share a datastructure [Shared_meta.t] that is only created
    once meta-info is known.

    A swarm can be in two states, determined by the presence of [Shared_meta.t]. 
    - without meta-info (e.g. we download a magnet for the first time), [t] 
    tries to get meta-info.
    - with meta-info (that we got from a torrent, or downloaded from the peers), 
    [t] tries do download pieces. 

    TODO: currently, we unchoke all interested peers, but we should be able to
    restrict their number. *)

open Core
open Async

type t

(** [create u sm h]. [u] is the URL of the tracker. If not provided, peers
    are looked up with the DHT. If [sm] is not provided, it will be created
    later on by the swarm when meta-info is found from the peers. *)
val create: Uri.t option -> Shared_meta.t option -> Bt_hash.t -> t 

val start: t -> unit

(** not implemented yet *)
val stop : t -> unit

(** add peers to the swarm. Peers completed handshake. The returned deferred
    is determined when the remote peers cut the connection or when the swarm
    disconnect it.

    TODO: add a phantom type to marks that peers indeed completed their 
    handshake? *)    
val add_peer_comm: t -> Peer_comm.t -> Peer_comm.handshake_info 
  -> unit Deferred.t

(** saves the persistent stuff and disconnect peers *)
val close: t -> unit Deferred.t

val status : t -> Status.t