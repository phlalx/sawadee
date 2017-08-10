(** Client-specific global constant *)

open Core
open Async

val meta_block_size : int 

(** 16KB, we ignore other requests *)
val block_size : int

(** The max block_size with authorize *)
val max_block_size : int

val idle : Time.Span.t  

val keep_alive : Time.Span.t

val is_server : unit -> bool 

val port_exn : unit -> int

val set_download_path : string -> unit

val set_torrent_path : string -> unit

val set_dht_port : int -> unit

val set_port : int -> unit

val max_pending_request : int 

(** max number of peers we accept to serve *)
val max_non_choking_peers : int

val peer_id : Peer_id.t

val routing_table_name : string

val dht_port_exn : unit -> int

val is_dht : unit -> bool

val max_num_pieces : int

val log_name : string

val with_torrent_path : string -> string

val with_download_path : string -> string

val bitset_name : string -> string

val torrent_name : string -> string

val client_id : string

val max_unchoke : int


