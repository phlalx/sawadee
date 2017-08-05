(** Client-specific global constant *)

open Core
open Async

(** 16KB, we ignore other requests *)
val block_size : int

(** The max block_size with authorize *)
val max_block_size : int

val idle : Time.Span.t  

val keep_alive : Time.Span.t

val is_server : unit -> bool 

val download_path : unit -> string

val torrent_path : unit -> string

val port_exn : unit -> int

val set_download_path : string -> unit

val set_torrent_path : string -> unit

val set_port : int -> unit

val max_pending_request : int 

(** max number of peers we accept to serve *)
val max_non_choking_peers : int

(** extension to serialize bitfield ".bitset" *)
val bitset_ext : string

(** extension to save torrents ".torrent" *)
val torrent_ext : string

val peer_id : Peer_id.t

val node_id : Node_id.t

val routing_table_name : string

val krpc_timeout : Time.Span.t

val set_node : bool -> unit

val is_node : unit -> bool

val max_num_pieces : int

