(** P2P layer of the protocol. 

    Type [t] is used to maintain state related to a peer and communication 
    functions. This is exclusively used by [App_layer] module peer. *)

open Core
open Async

type t 

(** [Create addr ~piece_num] tries to open the connexion with a peer. 
    [piece_num] is the number of pieces in the file to be downloaded *)
val create: Socket.Address.Inet.t -> piece_num:int -> (t,exn) result Deferred.t 

(** Used to identify peers in log "IP/PORT" - but could be peer_id *)
val to_string : t -> string

(** Networking functions *)

(** [handshake x h pid] tries to handshake with peer [x] using the hash from 
    the metainfo file [h] and the identifier of the client [pid]. As a side
    effect, it sets the peer_id of the remote peer. This has to be called
    before [send_message] or [receive_message]. *) 
val handshake: t -> Bt_hash.t -> Peer_id.t -> (unit, exn) result Deferred.t

val get_message : t -> Message.t Reader.Read_result.t Deferred.t

val send_message : t -> Message.t -> unit

(** [t] maintains the set of pieces owned by peer. Pieces are referred to by 
    their indexes *)

val has_piece : t -> int -> bool

val owned_pieces : t -> Bitset.t

(** Sets the bitfield describing the list of pieces owned by peer. We use 
    the format defined in the bittorrent protocol.  *)

val set_owned_pieces : t -> Bitfield.t -> unit

val set_owned_piece : t -> int -> unit

(** to be called at every [Global.tick]. Inform App_layer about this peer 
    responsivness.

    If [`Idle l], peer is marked as idle, and pieces in `l` have to be 
    re-requested by App_layer. Pending requests are erased on peer. 

    If [`Keep_alive], a keep alive message has to be sent.

    TODO: this would be better using time-out, but it's simple this way *)
val tick : t -> [ `Idle of int list | `Ok | `Keep_alive ]

(** The following functions simply get/set one bit states. *)

val is_peer_interested : t -> bool

val is_peer_choking : t -> bool

val am_interested : t -> bool

val am_choking : t -> bool

val set_peer_interested : t -> bool -> unit

val set_peer_choking : t -> bool -> unit

val set_am_interested : t -> bool -> unit

val set_am_choking : t -> bool -> unit

(** unresponsive peers become idle, we don't request them anymore pieces. *)
val is_idle : t -> bool

(** Maintain a set of pending (index of) piece requests. This has to be
    done in order to re-request them if a peer becomes unresponsive *)

val add_pending: t -> int -> unit

val remove_pending: t -> int -> unit

(** assert a condition dependent on values received by a peer.
    raises if false TODO: close connection with peer instead *) 
val validate : t -> bool -> unit

(** display stats for debugging *)
val stats : t -> unit
