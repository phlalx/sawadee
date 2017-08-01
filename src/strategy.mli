(** Implements a downloading strategy.

    Based on peers and pieces information, decides what piece to download
    from what peer.  *)

open Core

type pi = Peer.t * State.t 

(* TODO change this interface. *)
val next_requests : File.t -> (Peer_id.t, pi) Hashtbl.t -> int -> 
(int * pi) list