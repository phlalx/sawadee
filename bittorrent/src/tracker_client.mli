(** Http client to query the tracker. *)

open Core
open Async

(** Retrieve list of peer addresses from trackers given in announce(s). 

  TODO: deal with error. We just return an empty list of address if error *)
val query: Bt_hash.t -> Uri.t list -> Addr.t list Deferred.t

