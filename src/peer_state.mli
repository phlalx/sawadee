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
  mutable id : Peer_id.t;
  mutable idle : bool;
}

val create : Peer.t -> t

val peer : t -> Peer.t

val id : t -> Peer_id.t

val to_string : t -> string

(** maximal number of owned pieces, should be called right after handshake 

    Note that this isn't necessarily known at creation time. In server mode, 
    we don't know what torrent the peer is going to request. This is not the 
    case now, but we could be serving more than one torrent *)
val init_size_owned_pieces : t -> int -> unit

(** unresponsive peers become idle, we don't request them anymore pieces. *)
val is_idle : t -> bool

val set_idle : t -> bool -> unit

(** [t] maintains the set of pieces owned by peer. Pieces are referred to by 
    their indexes. These `owned pieces` are updated upon message reception
    (specifically, [Bitfield] and [Have]) *)

val has_piece : t -> int -> bool

val owned_pieces : t -> Bitset.t

val set_owned_pieces : t -> Bitfield.t -> unit

val set_owned_piece : t -> int -> unit

(** The following functions are getter/setters for the boolean states 
    defined by the spec *)

val is_peer_interested : t -> bool

val is_peer_choking : t -> bool

val am_interested : t -> bool

val am_choking : t -> bool

val set_peer_interested : t -> bool -> unit

val set_peer_choking : t -> bool -> unit

val set_am_interested : t -> bool -> unit

val set_am_choking : t -> bool -> unit

(** [t] maintains a set of pending (index of) piece requests. This has to be
    done in order to re-request them if a peer becomes unresponsive. *)
val add_pending: t -> int -> unit

val remove_pending: t -> int -> unit

val get_pending : t -> int list 