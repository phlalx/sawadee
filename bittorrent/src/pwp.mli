(** Peer wire protocol.  *)

open Core
open Async

type t

val create: Bt_hash.t -> t

val set_nf : t -> Torrent.info -> unit Deferred.t

val add_peer: t -> Peer.t -> unit Deferred.Or_error.t 

val close: t -> unit Deferred.t

val status : t -> Status.t