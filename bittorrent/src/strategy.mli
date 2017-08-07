(** Implements a downloading strategy.

    Based on peers and pieces information, decides what piece to download
    from what peer.  *)

open Core

(** [next_requests l p n] returns a list of pieces to download from what peer.
    [l] is the list of pieces not downloaded and not requested yet, and
     p is the list of peers. [n] is the number of pieces we want to download *)
val next_requests : int list -> Peer.t list -> int -> (int * Peer.t) list