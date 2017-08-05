
include Hash_id.ID

val distance : t -> t -> int

val distance_hash : t -> Bt_hash.t -> int

(* [compare h n1 n2] < 0 if n1 is closest to h than n2, = 0 is same dist *)
val compare : Bt_hash.t -> t -> t -> int