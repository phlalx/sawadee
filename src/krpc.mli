(** This DHT node. 

    Maintains routing table... *)

open Core
open Async
open Log.Global

val try_add : Unix.Inet_addr.t -> port:int -> unit 

(* serialize table *)
val table_to_string : (Node_id.t * Socket.Address.Inet.t) list  -> string 

val table_of_string : string -> (Node_id.t * Socket.Address.Inet.t) list 

val table : unit -> (Node_id.t * Socket.Address.Inet.t) list 