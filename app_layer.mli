open Core
open Async

type t = {
  file : File.t;
  peer : Peer.t; (* TODO eventually we'll have to keep track 
                    of a set of peers *)
  this_peer_id : string;
  choked : bool; (** do I serve request? initally false *)
  interested : bool; (** am I interested in file? initially true *)
}

val create: Socket.Address.Inet.t -> File.t -> string -> (t, exn) result Deferred.t

val init : t -> (unit, exn) result Deferred.t



