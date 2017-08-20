open Core
open Async

module Kp = Krpc_packet  

val send_packet : Addr.t -> Kp.t -> unit 