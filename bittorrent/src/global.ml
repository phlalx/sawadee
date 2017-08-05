open Core

let block_size = 16384 

let max_block_size = 131072

(** Time before a host is considered idle *)
let idle = sec 100.

let keep_alive = sec 180. 

let path_ = ref None

let port_ = ref None

let port_exn () = Option.value_exn !port_

let path () = Option.value_exn !path_

let set_port p = port_ := Some p 

let set_path p = path_ := Some p

let is_server () = Option.is_some !port_

let max_pending_request = 10 

let max_non_choking_peers = 4

let bitset_ext = "_bitset"

let peer_id = Peer_id.random ()

let node_id = Node_id.random ()

let routing_table_name = "routing"

let krpc_timeout = sec 5.0

let is_node_ = ref false

let is_node () = !is_node_

let set_node b = is_node_ := b

let max_num_pieces = 65536
