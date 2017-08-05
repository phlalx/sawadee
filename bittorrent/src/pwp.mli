(** Peer wire protocol. 

    This central module is responsible of sharing data with the peer via the
    peer wire protocol.

    To do so, it needs to maintain a bunch of objects. This includes:
    - the data being downloaded (see [File.t] and [Piece.t])
    - the [Peers.t] 
    - persistent data [Pers.t] *)

open Core
open Async

type t

val create: ?nf:Network_file.t -> unit -> t

(** Add new peers to communicate with. Connexion and handshake are already 
    established.

    This deferred is determined if peer leaves or fail *)
val add_peer: t -> Peer.t -> unit Deferred.Or_error.t 
