(** Read/write at a specific place in a file 

 This could be replaced by pwrite/rwrite but didn't manage to get them working
 in async. TODO *)
open Core
open Async

val enable_write : unit -> unit

val disable_write : unit -> unit

(** [read t s ps] read from t.fd at position t.off 
    and put result in string at position off % ps *)
val read : Fd.t -> Bigstring.t -> off:int -> pos:int -> len:int -> unit Deferred.t

(** [write t s ps] write to t.fd at position t.off 
    data from string at position off % ps *)
val write : Fd.t -> Bigstring.t -> off:int -> pos:int -> len:int -> unit Deferred.t



