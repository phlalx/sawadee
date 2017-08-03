open Core
open Async
open Log.Global

type t = {
  peer : Peer.t;
  mutable peer_choking : bool; 
  mutable peer_interested : bool;
  mutable am_choking : bool;
  mutable am_interested : bool; 
  mutable have : Bitset.t;
  mutable pending : Int.Set.t;
  mutable idle : bool;
  mutable port : int Option.t;
  mutable bitfield : Bitfield.t Option.t;
  mutable metadata_id : int Option.t;
  mutable metadata_size : int Option.t
} [@@deriving fields]
(* TODO experimenting with this extension... see if it's worth it *)

val create : Peer.t -> t

val id : t -> Peer_id.t

val to_string : t -> string

(** maximal number of owned pieces, should be called right after handshake 

    Note that this isn't necessarily known at creation time. In server mode, 
    we don't know what torrent the peer is going to request. This is not the 
    case now, but we could be serving more than one torrent *)
val init_size_owned_pieces : t -> int -> unit

(** [t] maintains the set of pieces owned by peer. Pieces are referred to by 
    their indexes. These `owned pieces` are updated upon message reception
    (specifically, [Bitfield] and [Have]) *)

val has_piece : t -> int -> bool

val owned_pieces : t -> Bitset.t

val set_owned_pieces : t -> Bitfield.t -> unit

val set_owned_piece : t -> int -> unit

(** [t] maintains a set of pending (index of) piece requests. This has to be
    done in order to re-request them if a peer becomes unresponsive. *)
val add_pending: t -> int -> unit

val remove_pending: t -> int -> unit
