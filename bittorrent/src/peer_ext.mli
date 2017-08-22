open Core
open Async

module Nf = Network_file

type t 

val create : Bt_hash.t -> Peer_comm.t -> Pevent.t Pipe.Writer.t -> Nf.t option 
  -> t

val send_handshake : t -> unit

val process_extended : t -> Extension.id -> Extension.t -> unit

val request_meta : t -> unit 

val close : t -> unit