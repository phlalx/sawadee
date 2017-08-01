(** Implements a downloading strategy.

    Based on peers and pieces information, decides what piece to download
    from what peer. 

    TODO: it would be better to specify the number of pieces we want (if more
    than one, we can avoid redoing expensive computation.
  
    At that stage, we don't have much information about peer (besides idleness).
    We would also need datastructure such as a map from pieces to list of
    peers. *)

open Core

type pi = Peer.t * State.t 

(* returns list of piece_index and peer_d *)
(* TODO this is only temporary while we refactor peer/state *)
val next_requests : File.t -> (Peer_id.t, pi) Hashtbl.t -> int -> 
(int * pi) list