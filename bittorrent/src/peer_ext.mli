(** Extension protocol.

    Each [Peer.t] that supports the extension creates a [t] value to deal with
    the extension protocol aspects. *)

open Core
open Async

type t 

(** Events are forwarded to the upper layer via the pipe. 
    Two type of events are relevant. 
    - remote peer supports the meta data extension
    - reception of meta data. *)
val create : Bt_hash.t -> Peer_comm.t -> Pevent.t Pipe.Writer.t -> 
  Shared_meta.t option -> t

(** The first message sent by the peer should be an extension handshake 
    that informs the remote peer about the extensions supported. None if 
    [Shared_meta.t] isn't known, otherwise, we support the metadata extension 
    (TODO what id). *)
val send_handshake : t -> unit

(** [process_extended t id ext] is called for all extended messages received 
    by [Peer.t] *)
val process_extended : t -> Extension.id -> Extension.t -> unit

val request_meta : t -> unit 

val close : t -> unit