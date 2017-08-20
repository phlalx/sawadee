open Core
open Async
open Log.Global

module G = Global
module Pc = Peer_comm

type t = {
  wr : (Pc.t * Pc.handshake_info) Pipe.Writer.t;
  info_hash : Bt_hash.t;
  uris : Uri.t list option;
}

let ignore_error addr : unit Or_error.t -> unit =
  function 
  | Ok () -> () 
  | Error err -> 
      ()
      (* debug !"Peer_producer: can't connect to %{Addr} - %{sexp:Error.t}" addr err *)

let close_on_error (p : Pc.t) hi = 
  match%bind hi with
  | Ok x -> Ok x |> return 
  | Error err -> Pc.close p >>| fun () -> Error err

let handshake_and_push t addr = 
  let open Deferred.Or_error.Let_syntax in
  let%bind p = Pc.create_with_connect addr in
  let%map hi = Pc.initiate_handshake p t.info_hash |> close_on_error p in
  info !"Peer_producer: pushing_peer %{Pc}" p;
  Pipe.write_without_pushback t.wr (p, hi) 

let get_peers_from_tracker t uris = 
  don't_wait_for (
    info "Peer_producer: querying tracker";
    let%bind addrs = Tracker_client.query t.info_hash uris in
    let num_of_peers = List.length addrs in 
    info "Peer_producer: %d tracker peers" num_of_peers;
    let f addr = handshake_and_push t addr >>| ignore_error addr in
    Deferred.List.iter ~how:`Parallel addrs ~f
  )

let get_peers_from_dht t dht = 
  don't_wait_for (
    info "Peer_producer: querying DHT";
    let%bind addrs = Dht.lookup dht t.info_hash in  
    let num_of_peers = List.length addrs in 
    info "Peer_producer: %d DHT peers" num_of_peers;
    let f addr = handshake_and_push t addr >>| ignore_error addr in
    Deferred.List.iter ~how:`Parallel addrs ~f
  )

let create wr info_hash uris = { wr; info_hash; uris }

let start t : unit =
  Option.iter t.uris ~f:(get_peers_from_tracker t);
  let f dht = 
    Clock.every (sec 120.) (fun () -> get_peers_from_dht t dht) 
  in
  Option.iter (G.dht ()) f










