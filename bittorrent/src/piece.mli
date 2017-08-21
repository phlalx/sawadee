(**  Piece of a network file.

     A Piece is bigsubstring of the network file (see [Network_file.t]).  

     In bittorent, a piece is the unit of "ownership". Peers advertise and 
     request pieces. However, they are furthmore divided into blocks 
     (or sub-pieces)which are the unit of transmission. Block size is 
     client-specific (normally 16kB) and is defined [Global]. 

     Pieces also have a hash given in the meta-info and used to validate after
     downloading. *)

open Core
open Async

type t

(** [create ~pos ~index ~len h s]. [pos] is the offset of the piece in the
    network file. All pieces should have the same length exception maybe the 
    last one. *)
val create :  pos:int -> index:int -> len:int -> Bt_hash.t -> Bigstring.t -> t

val get_index : t -> int

(* blocks requested by peers must fit in a piece, and have a size less or
   equal than G.max_block_size *)
val is_valid_block_request : t -> off:int -> len:int -> bool

(* Copy part of the content to a string, to be used to create a piece
   message. TODO replace with a blit to the output buffer *)
val get_content : t -> off:int -> len:int  -> string 

(* used for R/W. Doesn't allocate a new bigstring *)
val get_bigstring_content : t -> Bigsubstring.t

val blocks : t -> Block.t list 

(** Updates a piece with downloaded block.

    This updates the content of the piece and check for consistency. Called 
    by [Swarm] upon reception of a block *)
val update : t -> off:int -> string -> [ `Downloaded | `Ok | `Hash_error ] 

val is_hash_ok : t -> bool

(** for logging *)
val to_string : t -> string
