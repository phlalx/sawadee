open Core
open Async
open Log.Global

type t

val create : 
  unit Mvar.Read_only.t -> (Peer_comm.t * Peer_comm.handshake_info) Pipe.Writer.t -> 
  Bt_hash.t -> Uri.t option -> t

val start : t -> unit
