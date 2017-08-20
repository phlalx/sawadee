open Core

type t = (Node_id.t, Addr.t list) Hashtbl.t

val create : unit -> t

val add : t -> Node_id.t -> Addr.t -> unit

val find : t -> Node_id.t -> Addr.t list