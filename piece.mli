open Core
open Async

val block_size : int

type t

val create : index:int -> Bt_hash.t -> len:int -> t

(* TODO probably can get rid of this, it's only used for pending set
   of pieces index in Peer.t. We can use a set of Piece.t instead *)
val get_index : t -> int

val iter : t -> f:(index:int -> off:int -> len:int -> unit) -> unit

(** updates piece with downloaded block *)
val update : t -> off:int -> string -> [ `Downloaded | `Ok | `Hash_error ]

val get_status : t -> [`Requested | `Downloaded | `Not_requested | `On_disk]

val set_status : t -> [`Requested | `Downloaded | `Not_requested | `On_disk] 
  -> unit

val to_string : t -> string

val write : t -> Writer.t -> unit

val file_offset : t -> int64
