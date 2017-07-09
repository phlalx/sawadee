open Core

val block_size : Int32.t

type t

val get_index : t -> int

val length : t -> int

val to_be_downloaded : t -> bool

val set_requested : t -> unit

val create : index:int -> hash:string -> len:int -> t

(** total number of block in a piece *)
val num_blocks : t -> int

(** offset and length of a block *)
val offset_length : t -> int -> Int32.t * Int32.t

(** updates piece with downloaded block *)
val update : t -> int -> string -> [ `Downloaded | `Ok | `Hash_error ]

val offset_to_index : Int32.t -> int