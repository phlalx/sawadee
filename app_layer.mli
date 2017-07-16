(** Application layer. 

  This module is responsible to download a file by querying the remote peers,
  and serve peers request. It maintains application state and implements i
  the *peer protocol. *)

open Core
open Async

type t

(** [create f p]. [f] is the file to be downloaded and [p] identifies
    the local peer *)
val create: File.t -> Peer_id.t -> t

(** Launch the *services* that listen for new messages, query the peers... 
    Does not much until peers are added. *)
val start: t -> unit

val stop: t -> unit

(** Add new peers to communicate with. Peer can be added dynamically. 
    This silently fails if connexion or handshake can't be established with 
    the peer. *)
val add_peer: t -> Socket.Address.Inet.t -> unit


