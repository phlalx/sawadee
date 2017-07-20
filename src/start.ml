open Core
open Async
open Log.Global
module G = Global

let peer_create peer_addr = 
  let wtc = Tcp.to_inet_address peer_addr in
  debug "trying to connect to peer %s" (Socket.Address.Inet.to_string peer_addr);
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
    | Ok peer -> Pwp.add_peer pwp peer 
    | Error err -> 
      let s = Socket.Address.Inet.to_string peer_addr in
      info "can't connect to peer %s" s) 
  in
  List.iter peer_addrs ~f;
  if G.is_server () then  (

    let handler pwp addr r w =
      let p = Peer.create addr r w `Peer_initiating in
      return (Pwp.add_peer pwp p)
    in 

    Server.start (handler pwp));
  Deferred.unit

let start_pwp peer_addrs num_pieces torrent_name info_hash files_info 
    pieces_hash piece_length total_length =
  let bitfield_name = (Filename.basename torrent_name) ^ G.bitset_ext in
  let bf_length = Bitset.bitfield_length_from_size num_pieces in 
  match%bind Pwp.create info_hash bitfield_name bf_length files_info pieces_hash
               G.peer_id piece_length total_length
  with 
  | Ok pwp -> add_peers pwp peer_addrs
  | Error err -> assert false

(** [process f] downloads files described by metainfo file [f]. *)
let process torrent_name =

  (* Decode torrent file *)

  let open Torrent in 
  let { info_hash; announce; announce_list; mode; pieces_hash; piece_length; 
        files_info }
    = Torrent.from_file torrent_name 
  in

  let num_pieces = Array.length pieces_hash in
  let num_files = List.length files_info in

  info "Torrent: %s:" torrent_name;
  info "Torrent: %d files" num_files;
  info "Torrent: %d pieces" num_pieces;
  info "Torrent: piece length = %d" piece_length;

  let total_length = List.fold files_info ~init:0 ~f:(fun acc (_,l) -> l + acc)
  in 

  Tracker_client.init announce announce_list info_hash total_length G.peer_id; 
  debug "trying to connect to tracker";
  match%bind Tracker_client.query () with
  | Ok peer_addrs -> 
    let num_of_peers = List.length peer_addrs in 
    info "tracker replies with list of %d peers" num_of_peers;
    start_pwp peer_addrs num_pieces torrent_name info_hash files_info
      pieces_hash piece_length total_length 

  | Error err -> 
    info "can't connect to tracker";
    flushed () >>= fun () ->
    exit 1