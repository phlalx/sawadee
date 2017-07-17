(** This module defines client-specific constant that are used elsewhere
    in the program. *)

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

(** path where to store downloaded file and bitfields 
    "./download/" *)

val path : string

val max_pending_request : int 

(** max number of peers we accept to serve *)
val max_non_choking_peers : int

(** extension to serialize bitfield "_bitset" *)
val bitset_ext : string



