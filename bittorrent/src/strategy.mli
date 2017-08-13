(** Compute next pieces to request.  

This will eventually be replaced by a more efficient datastructure. *)

open Core

(** [next_requests l p n] returns pieces to download. [l] is the list of pieces 
    not downloaded and not requested yet, p is the list of peers, [n] is the 
    number of pieces we want to download *)
val next_requests : int list -> Peer.t list -> int -> (int * Peer.t) list