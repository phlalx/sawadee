open Core
open Async
open Log.Global 

let table = Hashtbl.Poly.create ()

(* let add_connected_peer pwp kind info_hash num_pieces addr r w  =
  let open Deferred.Or_error.Monad_infix in 
  let peer = P.create addr r w kind in
  P.handshake peer info_hash G.peer_id
  >>= fun () ->
  Print.printf "handshake with (server) peer %s\n" (P.addr_to_string peer);
  P.init_size_owned_pieces peer num_pieces;
  Pwp.add_peer pwp peer

(* TODO must be a more elegant way of combining these monads *)
let add_connected_peer_and_close pwp kind info_hash num_pieces addr r w =
  let close x = Writer.close w >>= fun () -> Reader.close r >>| fun () -> x in
  add_connected_peer pwp kind info_hash num_pieces addr r w 
  >>= close
 *)  
let add info_hash pwp = ignore (Hashtbl.add table ~key:info_hash ~data:pwp)

let handler addr r w =
  info "incoming connection on server";
  let peer = Peer.create addr r w `Peer_initiating in


  Deferred.unit 

let start ~port = 
  info "listening on port %d" port;
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port port)
      handler
  in
  Deferred.ignore host_and_port


(*


(* TODO must be a more elegant way of combining these monads *)
let add_connected_peer_and_close pwp kind info_hash num_pieces addr r w =
  let close x = Writer.close w >>= fun () -> Reader.close r >>| fun () -> x in
  add_connected_peer pwp kind info_hash num_pieces addr r w 
  >>= close

*)
  (*


  let handler addr r w =
    info "incoming connection on server from peer %s" 
      (Socket.Address.Inet.to_string addr);
    add_connected_peer_and_close pwp `Peer_initiating info_hash num_pieces addr r w 
    >>| ignore_error addr 
  in 

  Server.start handler ~port:(G.port_exn ())

  *)