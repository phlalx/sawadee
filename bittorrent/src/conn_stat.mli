open Core
open Async

type t

val create : unit -> t 

val close : t -> unit

val total_dl : t -> int

val total_ul : t -> int

val dl_speed : t -> float

val ul_speed : t -> float

val incr_dl : t -> int -> unit

val incr_ul : t -> int -> unit
