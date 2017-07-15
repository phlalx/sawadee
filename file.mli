(** File to be downloaded by the P2P protocol.

    All the information to create a file come from the metainfo file. A file
    is divided into [Piece.t] that are created upon creation of the file
    (see [Piece.t] doc). *)

open Core
open Async

type t

(** [create ~len h ph ~name ~pl] creates a new file. This include creating all 
    the file pieces [Piece.t] and creating the persistent (this explains the 
    deferred return type). 
  
    - [h] is the hash of the info section of the metainfo file.
    - [ph] is an array of hashed of each of individual pieces.

    TODO: 
    - this works only for an individual file.
    - do all the pieces have the same length? check that *)

val create : len:int -> Bt_hash.t -> (Bt_hash.t Array.t) -> name:string 
  -> piece_length:int -> t Deferred.t

val write_to_disk : t -> unit 

val get_piece : t -> int -> Piece.t

val num_pieces : t -> int

val set_piece_have : t -> int -> unit

val num_piece_have : t -> int

(** [hash t] is the hash of the info section of the metainfo file, used in
    handshaking *)
val hash : t -> Bt_hash.t

(** This is the bitset decribing the list of pieces we have, in the format
    specified by the protocol. Can be sent as is in [Bitfield] messages *)
val bitset : t -> string
