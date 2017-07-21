(* Simple generic server *)

open Core

(** register a handler that is called on an incoming connexion *)
val start :  (Async.Socket.Address.Inet.t -> Async.Reader.t -> Async.Writer.t -> 
              unit Async_kernel__Deferred.t) -> port:int -> unit