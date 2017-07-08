open Core
open Async

type t = {
  file : File.t;
  mutable peer : Peer.t list;
  this_peer_id : string;
  choked : bool; (** do I serve request? initally false *)
  interested : bool; (** am I interested in file? initially true *)
}

val create:  File.t -> string -> t

val start: t -> Socket.Address.Inet.t list -> unit Deferred.t


