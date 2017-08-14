open Core
open Async
open Log.Global

module G = Global

type t = {
  wr : (Peer_comm.t * Peer_comm.handshake_info) Pipe.Writer.t;
  info_hash : Bt_hash.t;
  uris : Uri.t list option;
}

let ignore_error addr : unit Or_error.t -> unit =
  function 
  | Ok () -> () 
  | Error err -> debug !"Pwp: can't connect to %{Addr}" addr

let handshake_and_push t addr = 
  let open Deferred.Or_error.Let_syntax in
  let%bind p = Peer_comm.create_with_connect addr in
  let%map hi = Peer_comm.initiate_handshake p t.info_hash in
  Pipe.write_without_pushback t.wr (p, hi)

let get_peers_from_tracker t uris = 
  don't_wait_for (
    let%bind addrs = Tracker_client.query t.info_hash uris in
    let num_of_peers = List.length addrs in 
    info "Pwp: %d tracker peers" num_of_peers;
    let f addr = handshake_and_push t addr >>| ignore_error addr in
    Deferred.List.iter ~how:`Parallel addrs ~f
  )

let get_peers_from_dht t dht = 
  don't_wait_for (
    let%bind addrs = Dht.lookup dht t.info_hash in  
    let num_of_peers = List.length addrs in 
    info "Pwp: %d DHT peers" num_of_peers;
    let f addr = handshake_and_push t addr >>| ignore_error addr in
    Deferred.List.iter ~how:`Parallel addrs ~f
  )

let create wr info_hash uris = { wr; info_hash; uris }

let start t : unit =
  Option.iter t.uris ~f:(get_peers_from_tracker t);
  Option.iter (G.dht ()) (get_peers_from_dht t)
