open Core

val block_size : Int32.t

type t

val get_index : t -> int

val to_be_downloaded : t -> bool

val set_requested : t -> unit

val create : int -> string -> int -> t

(** updates pieces with downloaded block *)
val update : t -> Int32.t -> string -> [ `Downloaded | `Ok | `Sha_error ]

(** generate offset / length of missing pieces *)
val blocks : t -> (Int32.t * Int32.t) list
