(**  Piece of a (network) file.

     They are created upon creation of a [File.t]. A piece is furthermore 
     divided into blocks. A piece is the unit of "ownership". That is, peer 
     advertise and request pieces. A Block is the unit of transmission in the 
     peer protocol. Blocks are exposed to client modules through the [iter] 
     and [update] functions. Block size is defined globally in [Global]. *)

open Core
open Async

type t

(** [create i h ~len pfs] creates a piece at index [i] in the file with hash [h]
    and length [len]. *)
val create : index:int -> Bt_hash.t -> len:int -> t

val get_index : t -> int

(* Copy content to a string, to be used to create a [Piece] message *)
val get_content : t -> off:int -> len:int  -> string 

(* a received block should match an off/len that this client has requested *)
val is_valid_block : t -> off:int -> len:int -> bool

(* peer may not have the same block size, but requested blocks should belong to
   the piece *)
val is_valid_block_request : t -> off:int -> len:int -> bool

(* use for R/W. Doesn't allocate a new bigstring *)
val get_bigstring_content : t -> Bigstring.t

(** Iter through the blocks of the piece. Typically to send them to the peers.

    In this implementation, we always request all blocks at the same time
    to the same peer. In function [f], [index] is bound the the index of the 
    piece and is thus constant, [off] is the offset of the block, and [len]
    its length. Note that all blocks have the same length except possibly the
    last one *)
val iter : t -> f:(index:int -> off:int -> len:int -> unit) -> unit

(** Updates a piece with downloaded block.

    This updates the content of the piece and check for consistency. Called 
    by [Pwp] upon reception of a block.

    Possible returned values:
    - [`Downloaded] all blocks have been received and the hash of the pieces 
      match the expected hash from the metainfo file. 
    - [`Ok] the block was received but wasn't the last block. 
    - [`Hash_error] all blocks have been received but hashes don't match. *)
val update : t -> off:int -> string -> [ `Downloaded | `Ok | `Hash_error ] 

val is_hash_ok : t -> bool

(** for logging purpose *)
val to_string : t -> string
