open Core

type t = {
  block_size : int;
  max_block_size : int;
  idle : Time.Span.t;
  keep_alive : Time.Span.t;
  mutable download_path : string option;
  mutable torrent_path : string option;
  mutable port : int option;
  mutable dht_port : int option;
  max_pending_request : int;
  max_non_choking_peers : int;
  bitset_ext : string;
  torrent_ext : string;
  routing_table_name : string;
  mutable is_node : bool;
  max_num_pieces : int;
  meta_block_size : int;
  log_name : string;
  client_id : string;
  max_unchoke : int;
}

let t = {
  log_name = "log";
  block_size = 16384;
  max_block_size = 131072;
  idle = sec 50.; (** Time before a host is considered idle *)
  keep_alive = sec 10.;
  download_path = None;
  torrent_path = None;
  port = None;
  dht_port = None;
  max_pending_request = 1;
  max_non_choking_peers = 4;
  bitset_ext = ".bitset";
  torrent_ext = ".torrent";
  routing_table_name = "routing";
  is_node = false;
  max_num_pieces = 65536;
  meta_block_size = 16384;
  client_id = "-sW0010";
  max_unchoke = 4;
}

let log_name = t.log_name
let meta_block_size = t.meta_block_size
let max_num_pieces = t.max_num_pieces
let routing_table_name = t.routing_table_name
let peer_id = Peer_id.random ~prefix:t.client_id  ()
let torrent_ext = t.torrent_ext
let bitset_ext = t.bitset_ext
let max_non_choking_peers = t.max_non_choking_peers
let max_pending_request = t.max_pending_request
let keep_alive = t.keep_alive
let idle = t.idle 
let max_block_size = t.max_block_size
let block_size = t.block_size

let port_exn () = Option.value_exn t.port

let dht_port_exn () = Option.value_exn t.dht_port

let download_path () = Option.value_exn t.download_path

let torrent_path () = Option.value_exn t.torrent_path

let set_port p = t.port <- Some p 

let is_server () = Option.is_some t.port

let set_dht_port p = t.dht_port <- Some p 

let is_dht () = Option.is_some t.dht_port

let set_download_path p = t.download_path <- Some p

let set_torrent_path p = t.torrent_path <- Some p

let is_node () = t.is_node

let set_node b = t.is_node <- b

let with_torrent_path f= sprintf "%s/%s" (torrent_path ()) f 

let with_download_path f= sprintf "%s/%s" (download_path ()) f 

let bitset_name s = s ^ bitset_ext

let torrent_name s = s ^ torrent_ext

let client_id = t.client_id

let max_unchoke = t.max_unchoke
