open Core
open Async

type t = Socket.Address.Inet.t 

val create : Unix.Inet_addr.t -> port:int -> t

val addr : t -> Unix.Inet_addr.t 

val port : t -> int

val to_string : t -> string