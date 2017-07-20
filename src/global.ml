open Core

let block_size = 16384 

(** 1.0s, unit of time used everywhere. E.g. to compute compute transmission 
    speeds or idleness *) 
let tick = sec 1.0

type tick = int

(** Time before a host is considered idle *)
let idle = 15

let keep_alive = 180

let default_path = "download/"

let path_ = ref default_path

let port_ = ref None

let port_exn () = Option.value_exn !port_

let path () = !path_

let set_port p = port_ := Some p 

let set_path p = path_ := p ^ "/" 

let is_server () = Option.is_some !port_

let max_pending_request = 5 

let max_non_choking_peers = 4

let bitset_ext = "_bitset"

let peer_id = Peer_id.random ()

