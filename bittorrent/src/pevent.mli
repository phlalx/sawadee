(** Events produce by [Peers.t] and consumed by [Swarm.t]. *)

type t = 
  | Support_meta
  | Tinfo of Torrent.info
  | Bye
  | Piece of int

val to_string : t -> string