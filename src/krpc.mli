(** This DHT node. 

    Maintains routing table... *)

open Core
open Async
open Log.Global

type t 

val create : unit -> t

val try_add : Unix.Inet_addr.t -> port:int -> unit 