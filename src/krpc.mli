(** This DHT node. 

    Maintains routing table... *)

open Core
open Async
open Log.Global

type t 

val create : unit -> t