(** P2P layer of the protocol. 

    Type [t] is used to: 
      - maintain state related to a peer
      - communicate with peers via high-level message send/receive function.

    The state consists notably:
      - protocol defined booleans (interested/choking)
      - the pieces owned by the peer
      - the pending requests emitted to a peer and not yet acknowledged *)

open Core
open Async

type t 

(** We suppose that the connection is already established and we have a reader
    and a writer to communicate with the peer. The address is only used to 
    identify the peer in debug traces.

    We distinguish between the peers that initiate the connexion (when our
    peer is listening to incoming connexion), and those that we contact
    from the tracker's list of peer. This makes only a difference in the 
    handshake. *)
val create : Socket.Address.Inet.t -> Reader.t -> Writer.t ->
[`Am_initiating | `Peer_initiating ] -> t

(** maximal number of owned pieces, should be called right after handshake 

    TODO: this would be better as a [create] parameter. *)
val init_size_owned_pieces : t -> int -> unit

(** Used to identify peers in log "IP/PORT" - but could be peer_id *)
val to_string : t -> string

(** Communication functions *)

(** [handshake x info_hash pid] initiates the pwt protocol with peer [x].
    
    It consists in a round-trip message of the form.
       fixed_prefix ^ info_hash ^ pid

    The connecting peer is the one initiating the connexion. Both info_hash
    must match. Each peer sends its peer_id, that should match the one returned
    by the tracker (if any... apparently there's none in compact form which
    is the one we're using). 

    As a side effect, we save the peer_id but don't validate it. *)
val handshake: t -> Bt_hash.t -> Peer_id.t -> (unit, exn) result Deferred.t

val get_message : t -> Message.t Reader.Read_result.t Deferred.t

val send_message : t -> Message.t -> unit

(** [t] maintains the set of pieces owned by peer. Pieces are referred to by 
    their indexes. These `owned pieces` are updated upon message reception
    (specifically, [Bitfield] and [Have]) *)

val has_piece : t -> int -> bool

val owned_pieces : t -> Bitset.t

val set_owned_pieces : t -> Bitfield.t -> unit

val set_owned_piece : t -> int -> unit

(** to be called at every [Global.tick]. Inform `Pwp` about this peer 
    responsivness.

    If [`Idle l], peer is set to idle. Pending requets are erased.
    `Pwp` will re-request them (to a non-idle peer).

    If [`Keep_alive], a keep alive message has to be sent by `Pwp`. Should
    be every 180s.

    TODO: this would probably be better to use a time-out, but it's pretty 
    simple this way *)
val tick : t -> [ `Idle of int list | `Ok | `Keep_alive ]

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

(** unresponsive peers become idle, we don't request them anymore pieces. *)
val is_idle : t -> bool

(** [t] maintains a set of pending (index of) piece requests. This has to be
    done in order to re-request them if a peer becomes unresponsive. *)

val add_pending: t -> int -> unit

val remove_pending: t -> int -> unit

(** assert a condition dependent on values received by a peer. For instance,
    if peer doesn't behave according to the protocol. raises if false 

    TODO: terminate peer silently instead *) 
val validate : t -> bool -> unit

(** display stats for debugging *)
val stats : t -> unit
