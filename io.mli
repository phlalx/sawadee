
open Core
open Async

(** [read t s ps] read from t.fd at position t.off 
    and put result in string at position off % ps *)
val read : Fd.t -> Bigstring.t -> off:int -> pos:int -> len:int -> unit Deferred.t

(** [write t s ps] write to t.fd at position t.off 
    data from string at position off % ps *)
val write : Fd.t -> Bigstring.t -> off:int -> pos:int -> len:int -> unit Deferred.t



