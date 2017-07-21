(** Http client to query the tracker. *)

open Core
open Async

(** Initialize client with data used in subsequent queries *)
val init : Torrent.t -> unit

(** Initial query, retrieve list of peer addresses. Module must be initialized 
    with [init] first. There are other info in the tracker response but 
    we currently only need the list of peers. 

    If [announce_list] given in [init] is not empty, we use it and query all
    the trackers in turn until we get an answer. This is not exactly what is
    described in http://bittorrent.org/beps/bep_0012.html. TODO *)
val query: unit -> (Socket.Address.Inet.t list) Or_error.t Deferred.t

