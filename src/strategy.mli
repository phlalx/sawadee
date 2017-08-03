(** Implements a downloading strategy.

    Based on peers and pieces information, decides what piece to download
    from what peer.  *)

open Core

val next_requests : File.t -> (Peer_id.t, Peer.t) Hashtbl.t -> int -> 
(int * Peer.t) list