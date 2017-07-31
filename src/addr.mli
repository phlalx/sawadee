open Core
open Async

include (module type of Socket.Address.Inet)

val to_compact : t -> string 

val of_compact : string -> t