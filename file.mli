(** File to be downloaded by the P2P protocol. *)
open Core

type t = {
  file : string; (** buffer containing the file *)
  len : int;
  num_pieces : int; (** number of pieces to be downloaded *)
  pieces_hash : string Array.t; (** hashes of each pieces **)
  pieces_downloaded : bool Array.t;
  sha : string;  (** sha1 of the info section of the bittorrent file *)
}

val create : len:int -> sha:string -> pieces:(string list) -> t
