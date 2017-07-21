open Core
open Async
open Log.Global 

let start handler = 
  
  let port = Global.port_exn () in
  info "waiting for connexion on port %d" port;
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port port)
      handler
  in
  Deferred.don't_wait_for 
    (Deferred.ignore (host_and_port : 
                        (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t))
