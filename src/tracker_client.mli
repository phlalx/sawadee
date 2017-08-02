(** Http client to query the tracker. *)

open Core
open Async

(** Retrieve list of peer addresses from trackers given in announce.

    We query all the trackers and get as many peers as we can *)
val query: Bt_hash.t -> Uri.t list -> Addr.t list Deferred.t

