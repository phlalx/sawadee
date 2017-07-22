(** Http client to query the tracker. *)

open Core
open Async

(** Retrieve list of peer addresses from trackers given in announce.

    We query all the trackers in turn until we get an answer.
    TODO try to follow spec http://bittorrent.org/beps/bep_0012.html. *)
val query: Torrent.t -> (Socket.Address.Inet.t list) Deferred.Option.t

