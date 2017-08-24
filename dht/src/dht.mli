(** Simple DHT service.

http://bittorrent.org/beps/bep_0005.html  

*)

open Core
open Async

type t

val create : port:int -> Node_id.t -> data_path:string -> verbose:int -> t

(* try to add an address to the table. Ping this address and if it's 
   a node, add it. *)
val try_add : t -> Addr.t -> unit Deferred.Or_error.t

val table : t -> Node_info.t list

val lookup : t -> ?populate:bool -> Bt_hash.t -> Addr.t list Deferred.t

val announce : t -> Bt_hash.t -> port:int -> unit
