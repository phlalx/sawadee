open Core
open Async
open Log.Global 

module Pc = Peer_comm
module G = Global

let handler addr r w =

  let ignore_error addr : unit Or_error.t -> unit =
    function 
    | Ok () -> () 
    | Error err -> 
      info !"Server: %{Addr} can't connect" addr; 
      debug !"%{Error.to_string_hum}" err
  in

  let handler_or_error () : unit Deferred.Or_error.t = 
    let open Deferred.Or_error.Let_syntax in 
    info "Server: incoming connection";
    let p = Peer_comm.create addr r w in
    let%bind hi = Pc.wait_handshake p Torrent_table.has_hash in
    info !"Server: %{Pc.to_string} handshake" p;
    let pwp = Torrent_table.find_exn hi.info_hash in
    Pwp.add_peer_comm pwp p hi |> Deferred.ok

  in handler_or_error () >>| ignore_error addr

let start ~port = 
  info "Server: started on port %d" port;
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port port)
      handler
  in
  Deferred.ignore host_and_port
