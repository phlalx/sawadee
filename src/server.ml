open Core
open Async
open Log.Global 

let start handler ~port = 
  info "waiting for connexion on port %d" port;
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port port)
      handler
  in
  Deferred.don't_wait_for (Deferred.ignore host_and_port)
