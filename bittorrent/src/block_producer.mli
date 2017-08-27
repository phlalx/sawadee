(** feed consumer with blocks to request.

    the producer tries to produce as many blocks as possible, as long as:
    - it is not choking, in which it waits to be unchoked
    - the peer has pieces that we want and are not yet requested 
    - it is not blocked on a pipe push_back. 
    
    TODO: can we give it access to a subset of the state of shared_meta? *) 

open Core
open Async

type t

val create : Block.t Pipe.Writer.t -> Bitfield.t -> t

val start : t -> Shared_meta.t -> unit

val set_choking : t -> bool -> unit

val close : t -> unit