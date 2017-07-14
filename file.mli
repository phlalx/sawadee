(** File to be downloaded by the P2P protocol. *)
open Core
open Async

type t = {
  len : int;
  name : string;
  num_pieces : int; (** number of pieces to be downloaded *)
  pieces : Piece.t Array.t;
  hash : string;  (** hash of the info section of the bittorrent file *)
  bitset : Bitset.t;
  file_fd : Unix.Fd.t;
  bitset_fd : Unix.Fd.t;
  piece_length : int;
}

val create : len:int -> hash:string -> pieces_hash:(string Array.t) 
  -> name:string ->  piece_length:int -> t Deferred.t

val write_to_disk : t -> unit 