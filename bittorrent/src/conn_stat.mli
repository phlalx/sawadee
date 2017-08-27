(** Compute connections stats based on a sliding window.

    Stats are updated with [incr_dl] and [incr_ul] and the module
    computes the proper values for total dl/ul and connection speed, based
    on a slide windows of 20s (TODO add constant).

    TODO: should we have one global 'tick' producer? *)

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
