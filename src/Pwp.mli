(** Application layer. 

    This module is responsible to download a file by querying the remote peers,
    and serve peers request. It maintains application state and implements i
    the *peer protocol. *)

open Core
open Async

type t

val create: 
  Bt_hash.t 
  -> string -> int 
  -> (string * int) list  
  -> Bt_hash.t Array.t
  -> Peer_id.t 
  -> int (* Piece length *)
  -> int (* total length *)
  -> t Deferred.t

(** Launch the *services* that listen for new messages, query the peers... 
    Does not much until peers are added. *)
val start: t -> unit

val stop: t -> unit

(** Add new peers to communicate with. Peer can be added dynamically. 
    This silently fails if connexion or handshake can't be established with 
    the peer. *)
val add_peer: t -> Socket.Address.Inet.t -> unit


  (* let total_length = File.length file in *)

  (* let len = List.fold files ~init:0 ~f:(fun acc (_,l) -> l + acc) in  *)