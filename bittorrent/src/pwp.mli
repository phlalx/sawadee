(** Peer wire protocol / Bittorrent swarm.

  This module implements the peer wire protocol. It forms with [Peer] the 
  core of the bittorrent protocol.

  A [t] is created for each torrent to be downloaded. It acts as a conductor for
  a set of [Peer.t] thare is dynamically updated. It also contains all the state
  of the protocol (spread in [t] and in the [Peers.t]).

  [t] delegates most of the work to the [Peer.t] and communicate with them
  via messages. However, [t] shares a [Network_file.t] with the [Peer.t]. This
  allows the [Peer.t] to answers requests from the remote peers without 
  interacting with [t].

  One difficulty introduced with magnets is that we may not know the  meta-
  information [Torrent.info] at creation time, and we need to interact with
  remote peers to retrieve it.

  Hence, there are two states in [t]. 
  - without meta-info, [t] tries to get meta-info
  - with meta-info, [t] tries do download pieces.

  When dealing with a torrent, we immediately starts in the second state.

  The process of finding peers is done outside this module e.g. in 
  [Start] or [Server]. *)

open Core
open Async

type t

val create: Bt_hash.t -> t

val start: t -> Torrent.info option -> unit

(* Currently, peers can come from three sources:
  - the tracker
  - DHT
  - incoming peers from the server

  Handshake has been established when peers are added, but no other message
  has been sent.

  This function returns when [Peer.t] is not needed anymore. Resource freeing
  is then done by the caller. *)
val add_peer: t -> Peer.t -> unit Deferred.t 

(* close the network file if there is one *)
val close: t -> unit Deferred.t

val status : t -> Status.t