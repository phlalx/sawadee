(** Client-specific global constant 

    TODO : put this in a JSON file. *)

open Core
open Async

(** 16KB, we ignore other requests *)
val block_size : int

(** 1.0s, unit of time used everywhere. E.g. to compute compute transmission 
    speeds or idleness *) 
val tick : Time.Span.t  

type tick = int

(** Time before a host is considered idle
    TODO is this needed *)
val idle : tick 

(** Time before sending a keep-alive *)
val keep_alive : tick

val is_server : unit -> bool 

val path : unit -> string

val port_exn : unit -> int

val set_path : string -> unit

val set_port : int -> unit

val max_pending_request : int 

(** max number of peers we accept to serve *)
val max_non_choking_peers : int

(** extension to serialize bitfield "_bitset" *)
val bitset_ext : string

val peer_id : Peer_id.t


