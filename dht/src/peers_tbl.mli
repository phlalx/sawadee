open Core

type t = (Bt_hash.t, Addr.t list) Hashtbl.t

val create : unit -> t

val add : t -> Bt_hash.t -> Addr.t -> unit

val find : t -> Bt_hash.t -> Addr.t list