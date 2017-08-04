open Core
open Async
open Log.Global 

module P = Peer_comm
module G = Global

let table = Hashtbl.Poly.create ()

let add info_hash pwp = Hashtbl.add table ~key:info_hash ~data:pwp |> ignore

let handler addr r w =

  let ignore_error addr : unit Or_error.t -> unit =
    function 
    | Ok () -> () 
    | Error err -> 
      info !"Server: %{Addr} can't connect" addr; 
      debug !"%{Error.to_string_hum}" err
  in

  let handler_or_error () : unit Deferred.Or_error.t = 
    let open Deferred.Or_error.Monad_infix in 
    info "Server: incoming connection";
    let p = Peer_comm.create addr r w in
    let has_hash = Hashtbl.mem table in
    P.wait_handshake p has_hash
    >>= fun { info_hash; dht; extension } ->
    info !"Server: %{P.to_string} handshake" p;
    let pwp = Hashtbl.find_exn table info_hash in
    let peer = Peer.create p ~dht ~extension in
    Pwp.add_peer pwp peer 
    >>= fun () ->
    P.close p |> Deferred.ok

  in  handler_or_error () >>| ignore_error addr

let start ~port = 
  info "Server: started on port %d" port;
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port port)
      handler
  in
  Deferred.ignore host_and_port
