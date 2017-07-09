(** File to be downloaded by the P2P protocol. *)
open Core

type t = {
  len : int;
  name : string;
  num_pieces : int; (** number of pieces to be downloaded *)
  pieces : Piece.t Array.t;
  sha : string;  (** sha1 of the info section of the bittorrent file *)
  mutable pieces_downloaded : int;
}

val create : len:int -> sha:string -> pieces:(string list) -> name:string 
 ->  piece_length:int -> t
