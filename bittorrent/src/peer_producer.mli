open Core
open Async
open Log.Global

type t

val create : 
  (Peer_comm.t * Peer_comm.handshake_info) Pipe.Writer.t -> 
  Bt_hash.t -> 
  Uri.t list option -> t

val start : t -> unit