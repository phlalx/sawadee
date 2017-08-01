open Core
open Async
open Log.Global 

module P = Peer
module G = Global

let table = Hashtbl.Poly.create ()

let add info_hash pwp = ignore (Hashtbl.add table ~key:info_hash ~data:pwp)

let handler addr r w =

  (* TODO duplicated with start.ml, is there an api function for this? *)
  let ignore_error addr : unit Or_error.t -> unit =
    function 
    | Ok () -> () 
    | Error err -> 
      info "Error connecting with peer %s" (Addr.to_string addr);
      debug "Error connecting %s" (Sexp.to_string (Error.sexp_of_t err))
  in

  let handler_or_error () : unit Deferred.Or_error.t = 
    let open Deferred.Or_error.Monad_infix in 
    info "incoming connection on server";
    let peer = Peer.create addr r w in
    let has_hash = Hashtbl.mem table in
    P.wait_handshake peer has_hash G.peer_id
    >>= fun info_hash ->
    Print.printf "handshake with (server) peer %s\n" (P.addr_to_string peer);
    let pwp = Hashtbl.find_exn table info_hash in
    Pwp.add_peer pwp peer
    >>= fun () ->
    Peer.close peer |> Deferred.ok

  in  handler_or_error () >>| ignore_error addr

let start ~port = 
  info "listening on port %d" port;
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port port)
      handler
  in
  Deferred.ignore host_and_port
