open Core
open Async

type t = {
  file : File.t;
  peer : Peer.t; (* TODO eventually we'll have to keep track 
                    of a set of peers *)
}

val create: Socket.Address.Inet.t -> File.t -> t Deferred.t

val init : t -> unit Deferred.t

