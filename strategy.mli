open Core

val next_request : File.t -> Peer.t list -> (Piece.t * Peer.t) Option.t 