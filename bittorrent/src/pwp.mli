(** Peer wire protocol. 

 This module implements the peer wire protocol. A [t] is created for each
 torrent to be downloaded.

 It contains a set of [Peer.t] and all the state pertaining to the protocol
 and to the network file.

 In order to download a network file, one needs:
   - a [Bt_hash.t], the hash of the info section
   - a [Torrent.info], the content of the info section

 When we start with a torrent file, we have both info. But with a magnet, we
 have only the hash. In that case, we find peers from the DHT and needs 
 to query them to get the info. *)

open Core
open Async

type t

val create: Bt_hash.t -> t

val start: t -> Torrent.info option -> unit Deferred.t

(* Peers can come from three sources:
  - querying the tracker
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