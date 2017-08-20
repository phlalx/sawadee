open Core

type t

type token = string

val add : t -> Node_id.t -> (token * Addr.t) -> unit

val iter : t -> f:((token * Addr.t) -> unit) -> unit 

val create : unit -> t