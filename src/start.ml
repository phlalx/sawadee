open Core
open Async
open Log.Global
module G = Global

open Torrent

let peer_create peer_addr = 
  let wtc = Tcp.to_inet_address peer_addr in
  debug "try connecting to peer %s" (Socket.Address.Inet.to_string peer_addr);
  try_with (function () -> Tcp.connect wtc)
  >>| function
  | Ok (_, r, w) -> 
    Ok (Peer.create peer_addr r w `Am_initiating)
  | Error err -> Error err

let add_peers pwp peer_addrs =
  Pwp.start pwp;
  (* register handler for ctrl-c *)
  let stop (_:Signal.t) = Pwp.stop pwp in
  Signal.handle Signal.terminating ~f:stop;
  let f peer_addr = (
    peer_create peer_addr
    >>> function 
    | Ok peer -> don't_wait_for (Pwp.add_peer pwp peer)
    | Error err -> 
      let s = Socket.Address.Inet.to_string peer_addr in
      info "can't connect to peer %s" s) 
  in
  List.iter peer_addrs ~f;
  if G.is_server () then  (
    let handler pwp addr r w =
      info "Received connection on server";
      let p = Peer.create addr r w `Peer_initiating  in
      Pwp.add_peer pwp p
    in 
    Server.start (handler pwp));

  Deferred.unit

let start_pwp t peer_addrs =
  match%bind Pwp.create t with 
  | Ok pwp -> add_peers pwp peer_addrs
  | Error err -> assert false

(** [process f] downloads files described by metainfo file [f]. *)
let process torrent_name =

  (* Decode torrent file *)

  let t = Torrent.from_file torrent_name in
  info "Torrent: %s:" t.torrent_name;
  info "Torrent: %d files" t.num_files;
  info "Torrent: %d pieces" t.num_pieces;
  info "Torrent: piece length = %d" t.piece_length;

  Tracker_client.init t;
  debug "trying to connect to tracker";
  match%bind Tracker_client.query () with
  | Ok peer_addrs -> 
    let num_of_peers = List.length peer_addrs in 
    info "tracker replies with list of %d peers" num_of_peers;
    start_pwp t peer_addrs

  | Error err -> 
    info "can't connect to tracker";
    flushed () >>= fun () ->
    exit 1