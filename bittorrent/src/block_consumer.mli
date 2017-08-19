open Core
open Async

type t

val create : Block.t Pipe.Reader.t -> Peer_comm.t -> t

val notify : t -> Block.t -> unit

val start : t -> unit

val pending_requests : t -> int list