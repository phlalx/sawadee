(**  Piece of a (network) file.

     They are created upon creation of a [File.t]. A piece is furthermore divided
     into blocks. A piece is the unit of "ownership". That is, peer advertise and
     request pieces. A Block is the unit of transmission in the peer protocol. We
     only  expose blocks to client modules through the [iter] and [update]
     functions. *)

open Core
open Async

(** [block_size] is the size chosen by the client to request blocks. *)

type t

(** [create i h ~len pfs] creates a piece at index [i] in the file with hash [h]
    (given in the metainfo file) and length [len]. [pfs] are the pfiles
    where this piece should be read/written.  *)
val create : index:int -> Bt_hash.t -> len:int -> t

(** return the index of a piece.

    TODO probably can get rid of function, it's only used for pending set
    of pieces index in Peer.t. We could use a set of Piece.t instead to 
    make things a little more abstract. *)
val get_index : t -> int

val get_content : t -> off:int -> len:int -> string 

val get_content2 : t -> string

(** Iter through the blocks of the piece. Typically to send them to the peers.

    In this implementation, we always request all blocks at the same time
    to the same peer. In function [f], [index] is bound the the index of the 
    piece and is thus constant, [off] is the offset of the block, and [len]
    its length. Note that all blocks have the same length except possibly the
    last one *)
val iter : t -> f:(index:int -> off:int -> len:int -> unit) -> unit

(** Updates a piece with downloaded block.

    This updates the content of the piece and check for consistency. Typically
    called by the application layer upon reception of a block.

    Possible returned values:
    - [`Downloaded] all blocks have been received and the hash of the pieces 
      match the expected hash from the metainfo file. 
    - [`Ok] the block was received but wasn't the last block. 
    - [`Hash_error] all blocks have been received but hashs don't match.
    - TODO other option: block was already received, block doesn't have the 
      right format... *)
val update : t -> off:int -> string -> [ `Downloaded | `Ok | `Hash_error ]

val get_status : t -> [`Requested | `Downloaded | `Not_requested | `On_disk]

val set_status : t -> [`Requested | `Downloaded | `Not_requested | `On_disk] 
  -> unit

(** for logging purpose *)
val to_string : t -> string
