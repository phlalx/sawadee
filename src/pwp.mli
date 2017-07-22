(** Peer wire protocol. 

    This central module is responsible of downloading a file by interacting 
    with the peers. 

    To do so, it needs to maintain a bunch of objects. This includes:
    - the data being downloaded (see [File.t] and [Piece.t])
    - the [Peers.t] 
    - reading/write things on disk via [Pers.t]

    TODO: maybe it would be better to keep [Pers.t] outside of this module. *)

open Core
open Async

type t

val create: Torrent.t -> File.t -> Pers.t -> t

(** Launch the *threads* that periodically request pieces and *tick* the peers to
    check for idleness. Does not much until peers are added. 

    TODO: this work by polling the peers to see if they have new pieces. Ideally
    the requesting thread should wake up with some events (e.g. 
    arrival of new pieces) *)
val start: t -> unit

(** Add new peers to communicate with. Connexion is already established.
    Try handshake with peer, and launches message-reading loop.

    This deferred is determined if peer fails.
    TODO add proper Error.t *)
val add_peer: t -> Peer.t -> unit Deferred.t 
