(** Worker thread to communicate with a remote peer. 

    This module implements a "worker thread" that communicates with a remote 
    peer, and controlled by a "master" [Pwp.t].

    A [Peer.t] communicates with [Pwp.t] in two ways.
    - non blocking function calls (e.g. requesting a piece, or sending a
    bitfield.  
    - by notifying it with events on a pipe.

    All [Peer.t] in a swarm and their controling [Pwp.t] eventually share a
    [Network_file.t]. This let them answer block queries from
    remote peers with no interaction with [Pwp.t]. *)

open Core
open Async
open Log.Global

type t

val create : Peer_id.t -> Peer_comm.t -> dht:bool -> extension:bool -> t

(** starts the message loop. should be done before any other operation. 

    This terminates either when:
     - remote peer closes connection
     - we call [close] *)
val start : t -> unit Deferred.t

(** close the event pipe and [Peer_comm.t] *)
val close : t -> unit Deferred.t 

val set_nf : t -> Network_file.t -> unit

type event = 
  (* only before nf is set *)
  | Support_meta
  | Tinfo of Torrent.info
  (* only after nf is set *)
  | Choke 
  | Unchoke
  | Interested
  | Not_interested 
  | Have of int
  | Bitfield
  | Piece of int
  | Fail of int
  | More

val event_to_string : event -> string

(* Pwp.t reads events on this reader.

   Before nf is set, the only messages we can receive are Support_meta and T_info.
   Remote host can send bitfield and have, but we retain to simplify treatment
   in [Pwp.t] *)
val event_reader : t -> event Pipe.Reader.t



val id : t -> Peer_id.t

val to_string : t -> string

val has_piece : t -> int -> bool

val bitfield : t -> Bitfield.t

val am_interested : t -> bool

val am_choking : t -> bool

val take_request : t -> bool

val peer_choking : t -> bool

(* has this peer more than what is downloaded *)
val is_interesting : t -> bool

(** assert a condition dependent on values received by a peer. For instance,
    if peer doesn't behave according to the protocol. raises if false 

    TODO: terminate peer silently instead (monitor?). *) 
val validate : t -> bool -> unit

val validate_bitfield : t -> unit

val status : t -> Status.peer_status

(** The following function may send messages to the remote peer *)

val request_meta : t -> unit

val send_bitfield : t -> Bitfield.t -> unit 

val send_have : t -> int -> unit

(** request a piece and directly fill [Network_file.t] *)
val request_piece : t -> int -> unit

val set_am_interested : t -> bool -> unit

val set_am_choking : t -> bool -> unit




