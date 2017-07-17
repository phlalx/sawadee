open Core

let block_size = 16384 

(** 1.0s, unit of time used everywhere. E.g. to compute compute transmission 
    speeds or idleness *) 
let tick = sec 1.0

type tick = int

(** Time before a host is considered idle *)
let idle = 15

let keep_alive = 180

let path = "./download/" 

let max_pending_request = 10

let max_non_choking_peers = 4

let bitset_ext = "_bitset"

