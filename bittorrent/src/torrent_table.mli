open Core

val add : Bt_hash.t -> Pwp.t -> unit

val has_hash : Bt_hash.t -> bool

val find : Bt_hash.t -> Pwp.t Option.t

val find_exn : Bt_hash.t -> Pwp.t 

val keys : unit -> Bt_hash.t list

val data : unit -> Pwp.t list