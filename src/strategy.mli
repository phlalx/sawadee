(** Implements a downloading strategy.

    Based on peers and pieces information, decides what piece to download
    from what peer. 

    TODO: it would be better to specify the number of pieces we want (if more
    than one, we can avoid redoing expensive computation.
  
    At that stage, we don't have much information about peer (besides idleness).
    We would also need datastructure such as a map from pieces to list of
    peers. *)

open Core

(* returns list of piece_index and peer *)
val next_requests : File.t -> Peer.t list -> int -> (int * Peer.t) list