(** This is a rudimentary tracker for testing locally.

    It doesn't check the query and always returns the same set of peers.
    127.0.0.1:6000
    127.0.0.1:6001
    127.0.0.1:6002 *)

open Core
open Async
open Log.Global 
open Cohttp
open Cohttp_async

let from_port p = 
  Socket.Address.Inet.create (Unix.Inet_addr.of_string "127.0.0.1") p 

let reply = Tracker_reply.{
  complete = 0;
  incomplete = 0;
  interval = 0;
  peers = List.map [6000; 6001; 6002] ~f:from_port
}

let callback ~body addr request = 
  info "Tracker is processing request";
  Tracker_reply.to_bencode reply |>
  Server.respond_string ~flush:true

let server () =
  Server.create ~on_handler_error:`Raise (Tcp.on_port 6969) callback

let () = 
  don't_wait_for (Deferred.ignore (server ()));
  never_returns (Scheduler.go ())


