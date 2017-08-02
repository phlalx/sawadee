(** This is a rudimentary tracker for testing locally. It remembers the ports and inet
    addresses of clients, and serves them to new clients. *)

open Core
open Async
open Log.Global 
open Cohttp
open Cohttp_async

type t = {
  mutable peers : Addr.t list
}

let state = {
  peers = [];
}

let callback ~body (addr : Addr.t) request = 
  info "tracker is processing request";
  let `Inet (inet_addr, _) = addr in

  let uri = Request.uri request in
  let port = Uri.get_query_param uri "port"  in

  let reply = 
    Tracker_reply.{
      complete = 0;
      incomplete = 0;
      interval = 0;
      peers = state.peers; 
    } 
  in
  let set_port p =
    let port = int_of_string p in
    let peer_addr = Addr.create inet_addr ~port in
    info !"added peer %{Addr}" peer_addr;
    state.peers <- List.dedup (peer_addr :: state.peers)
  in
  Option.value_map port ~default:() ~f:set_port;
  Tracker_reply.to_bencode reply |>
  Server.respond_string ~flush:true

let server () =
  Server.create ~on_handler_error:`Raise (Tcp.on_port 6969) callback

let () = 
  don't_wait_for (Deferred.ignore (server ()));
  never_returns (Scheduler.go ())


