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
  info !"Tracker_server: %{Addr} request" addr;
  let `Inet (inet_addr, _) = addr in

  let uri = Request.uri request in
  let port = Uri.get_query_param uri "port"  in

  let reply = 
    let open Tracker_reply in 
    match port with
    | Some _ -> Ok { interval = 2000; peers = state.peers }
    | None -> Error "missing port"
  in
  let set_port p =
    let port = int_of_string p in
    let peer_addr = Addr.create inet_addr ~port in
    info !"Tracker_server: %{Addr} added" peer_addr;
    state.peers <- List.dedup_and_sort (peer_addr :: state.peers)
  in
  Option.iter port ~f:set_port;
  info !"Tracker_server: %{Addr} responded" addr;
  Tracker_reply.to_string reply |>
  Server.respond_string ~flush:true

let server () =
  Server.create ~on_handler_error:`Raise (Tcp.on_port 6969) callback

let () = 
  don't_wait_for (Deferred.ignore (server ()));
  never_returns (Scheduler.go ())


