(* table to store torrents being downloaded *)

open Core

val add : Bt_hash.t -> Swarm.t -> unit

val has_hash : Bt_hash.t -> bool

val find : Bt_hash.t -> Swarm.t option

val find_exn : Bt_hash.t -> Swarm.t 

val keys : unit -> Bt_hash.t list

val data : unit -> Swarm.t list