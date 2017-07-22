open Core
open Async
open Log.Global
module G = Global
module Em = Error_msg

let wait_for_incoming_peers pwp =
  let handler pwp addr r w =
    info "incoming connection on server from peer %s"
      (Socket.Address.Inet.to_string addr);
    let p = Peer.create addr r w `Peer_initiating  in
    Pwp.add_peer pwp p
  in 
  if G.is_server () then  (
    let port = G.port_exn () in
    Server.start (handler pwp) ~port
  ) 

let register_termination_handler pwp =
  Signal.handle Signal.terminating ~f:(fun _ -> Pwp.stop pwp)

let add_peers_from_tracker pwp peer_addrs =

  let peer_create peer_addr = 
    let wtc = Tcp.to_inet_address peer_addr in
    debug "try connecting to peer %s" (Socket.Address.Inet.to_string peer_addr);
    try_with (function () -> Tcp.connect wtc)
    >>| function
    | Ok (_, r, w) -> Ok (Peer.create peer_addr r w `Am_initiating)
    | Error err -> Error err
  in

  let add_peer pwp peer_addr = 
    peer_create peer_addr
    >>= function 
    | Ok peer -> Pwp.add_peer pwp peer
    | Error err -> 
      Socket.Address.Inet.to_string peer_addr
      |> info "can't connect to peer %s";
      Deferred.unit
  in
  Deferred.List.iter ~how:`Parallel peer_addrs ~f:(add_peer pwp)

let start_pwp t peer_addrs =
  match%bind Pwp.create t with 
  | Ok pwp -> 
    Pwp.start pwp;
    wait_for_incoming_peers pwp;
    register_termination_handler pwp;
    add_peers_from_tracker pwp peer_addrs
  | Error err -> assert false 

let process f =

  let t = try 
      Torrent.from_file f
    with
    | Sys_error _ -> failwith (Em.wrong_file f)
    | Failure s -> failwith (Em.not_bencode f)
    | Bencode_utils.Bencode_error -> failwith (Em.wrong_bencode f)
    | ex -> raise ex
  in 

  let%bind peer_addrs = match%bind Tracker_client.query t with
    | Some peer_addrs -> return peer_addrs 
    | None -> failwith (Em.tracker_error ())
  in
 
  let num_of_peers = List.length peer_addrs in 
  info "tracker replies with list of %d peers" num_of_peers;
  start_pwp t peer_addrs









