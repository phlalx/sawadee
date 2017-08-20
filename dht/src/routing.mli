
type t 

val create : unit -> t

val add : t -> Node_info.t -> unit

val k_closest : t -> Bt_hash.t -> Node_info.t list

val find_node : t -> Node_id.t -> Node_info.t list 

val to_list : t -> Node_info.t list