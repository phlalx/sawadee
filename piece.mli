open Core
open Async

val block_size : int

type t

val get_index : t -> int

val create : index:int -> hash:string -> len:int -> t

val iter : t -> f:(index:int -> off:int -> len:int -> unit) -> unit

(** updates piece with downloaded block *)
val update : t -> off:int -> string -> [ `Downloaded | `Ok | `Hash_error ]

val get_status : t -> [`Requested | `Downloaded | `Not_requested | `On_disk]

val set_status : t -> [`Requested | `Downloaded | `Not_requested | `On_disk] 
  -> unit

val to_string : t -> string

val write : t -> Writer.t -> unit

val file_offset : t -> int64