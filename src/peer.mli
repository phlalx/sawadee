(** P2P layer of the protocol. 

    Type [t] is used to communicate with peers via high-level message 
    send/receive function. *)


open Core
open Async

type t 

val equals : t -> t -> bool

(** We suppose that the connection is already established and we have a reader
    and a writer to communicate with the peer. The address is only used to 
    identify the peer in debug traces.

    We distinguish between the peers that initiate the connexion (when our
    peer is listening to incoming connexion), and those that we contact
    from the tracker's list of peer. This makes only a difference in the 
    handshake. *)
val create : Addr.t -> Reader.t -> Writer.t -> t

val create_with_connect : Addr.t -> t Deferred.Or_error.t

(** Used to identify peers in log (readable peer_id) *)
val to_string : t -> string

(** peer socket addr to string *)
val addr_to_string : t -> string

val id : t -> Peer_id.t 

(** Communication functions *)

(** [handshake x info_hash pid] initiates the pwt protocol with peer [x].

    It consists in a round-trip message of the form.
       fixed_prefix ^ info_hash ^ pid

    The connecting peer is the one initiating the connexion. Both info_hash
    must match. Each peer sends its peer_id, that should match the one returned
    by the tracker (if any... apparently there's none in compact form which
    is the one we're using). 

    As a side effect, we save the peer_id *)
val initiate_handshake: t -> Bt_hash.t -> Peer_id.t -> unit Deferred.Or_error.t

(** [wait_handshake] is use in the server when we don't know the 
    info_hash the peer wants to download (it's announced in the handshake.
     We validate the handshake only if we serve this info_hash *)
val wait_handshake : t -> (Bt_hash.t -> bool) -> Peer_id.t 
-> Bt_hash.t Deferred.Or_error.t

val get_message : t -> Message.t Reader.Read_result.t Deferred.t

val send_message : t -> Message.t -> unit

(** assert a condition dependent on values received by a peer. For instance,
    if peer doesn't behave according to the protocol. raises if false 

    TODO: terminate peer silently instead *) 
val validate : t -> bool -> unit

val set_downloading : t -> unit

val set_uploading : t -> unit

val addr : t -> Unix.Inet_addr.t

val close : t -> unit Deferred.t

