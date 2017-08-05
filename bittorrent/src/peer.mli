open Core
open Async
open Log.Global

type t

type event = 
  | Choke 
  | Unchoke
  | Interested
  | Not_interested 
  | Have
  | Bitfield
  | Bye

val create : Peer_comm.t -> dht:bool -> extension:bool -> t

val set_nf : t -> Network_file.t -> unit

val start : t -> unit

val event_to_string : event -> string

val support_metadata : t -> int Deferred.Option.t

val request_metadata_size : t -> int Deferred.Option.t

val request_metadata : t -> len:int -> string Deferred.t

val set_am_interested : t -> bool -> unit

val set_am_choking : t -> bool -> unit

val am_interested : t -> bool

val am_choking : t -> bool

val peer_choking : t -> bool

val idle : t -> bool

val send_bitfield : t -> Bitfield.t -> unit 

val advertise_piece : t -> int -> unit

val request_piece : t -> int -> unit Deferred.t 

val read_event : t -> event Deferred.t 

val id : t -> Peer_id.t

val to_string : t -> string

val has_piece : t -> int -> bool

val bitfield : t -> Bitfield.t

(** assert a condition dependent on values received by a peer. For instance,
    if peer doesn't behave according to the protocol. raises if false 

    TODO: terminate peer silently instead (monitor?). *) 
val validate : t -> bool -> unit