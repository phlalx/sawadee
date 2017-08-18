open Core
open Async

type t 

val create : Peer_comm.t -> Pevent.t Pipe.Writer.t -> t

val process_extended : t -> Extension.id -> Extension.t -> unit

val request_meta : t -> unit 

val close : t -> unit