(* Simple generic server *)

open Core
open Async

val start : port:int -> unit Deferred.t

val add : Bt_hash.t -> Pwp.t -> unit