open Core
open Async
open Log.Global
module G = Global
module P = Peer
module Em = Error_msg

let stop bf_name file pers = 
  let stop_aux () =
    info "written to files: %d pieces" (File.num_owned_pieces file);
    debug "written to files: %s" (File.pieces_to_string file);
    Pers.write_bitfield bf_name (File.bitfield file);
    Pers.close_all_files pers >>= fun () ->
    exit 0
  in 
  don't_wait_for (stop_aux ())


(* Creates a server that wait for connection from peers.

   After handshake, peer is initialized and added to Pwp. *)
let wait_for_incoming_peers pwp torrent =

  let open Torrent in
  let { info_hash; num_pieces } = torrent in

  let handler addr r w =
    info "incoming connection on server from peer %s"
      (Socket.Address.Inet.to_string addr);
    let open Deferred.Or_error.Monad_infix in 
    let peer = Peer.create addr r w `Peer_initiating  in
    Peer.handshake peer info_hash G.peer_id
    >>= fun () ->
    Peer.init_size_owned_pieces peer num_pieces;
    Deferred.ok (Pwp.add_peer pwp peer) 
  in 

  let handler_ignore_error addr r w =
    handler addr r w 
    >>| function
    | Ok () -> () 
    | Error err -> 
      info "Error connecting with peer %s" (Socket.Address.Inet.to_string addr);
      debug "Error connecting %s" (Sexp.to_string (Error.sexp_of_t err))
  in

  if G.is_server () then  (
    let port = G.port_exn () in
    Server.start handler_ignore_error ~port
  ) 

let add_peers_from_tracker pwp torrent addrs =

  let open Torrent in 
  let { info_hash; num_pieces } = torrent in

  (* Add a peer for this torrent. The steps are:
      - connecting
      - creating the peer data structure
      - handshake
      - adding the peer

     Some of these steps can fail. Errors are eventually ignored, but 
     propagated with an Or_error monad *) 

  let add_peer addr = 
    let open Deferred.Or_error.Monad_infix in 
    let wtc = Tcp.to_inet_address addr in
    debug "try connecting to peer %s" (Socket.Address.Inet.to_string addr);
    Deferred.Or_error.try_with (function () -> Tcp.connect wtc)
    >>= fun (_, r, w) ->
    Deferred.ok (return (Peer.create addr r w `Am_initiating))
    >>= fun peer ->
    Peer.handshake peer info_hash G.peer_id
    >>= fun () ->
    Peer.init_size_owned_pieces peer num_pieces;
    Deferred.ok (Pwp.add_peer pwp peer) 
  in

  (* TODO check if there is an API function for this step *)
  let add_peer_ignore_error addr =
    add_peer addr 
    >>| function 
    | Ok () -> ()
    | Error err -> 
      info "Error connecting with peer %s" 
        (Socket.Address.Inet.to_string addr);
      debug "Error connecting %s" (Sexp.to_string (Error.sexp_of_t err))
  in

  Deferred.List.iter ~how:`Parallel addrs ~f:add_peer_ignore_error

exception Wrong_piece of int

let process f =

  (***** read torrent file *****)

  let t = try 
      Torrent.from_file f
    with
    | Sys_error _ -> failwith (Em.wrong_file f)
    | Failure s -> failwith (Em.not_bencode f)
    | Bencode_utils.Bencode_error -> failwith (Em.wrong_bencode f)
    | ex -> raise ex
  in 

  (***** get list of peers ****)

  let%bind addrs = match%bind Tracker_client.query t with
    | Some addrs -> return addrs 
    | None -> failwith (Em.tracker_error ())
  in
  let num_of_peers = List.length addrs in 
  info "tracker replies with list of %d peers" num_of_peers;

  (***** open all files (files to download + bitset) **********)

  let open Torrent in
  let { files_info; num_pieces; piece_length; torrent_name; total_length; 
        info_hash; pieces_hash } = t in

  let bf_name = (G.path ()) ^ (Filename.basename torrent_name) ^ G.bitset_ext in
  let bf_len = Bitset.bitfield_length_from_size num_pieces in 

  let%bind pers = Pers.create files_info num_pieces piece_length  in

  (**** read bitfield *****)

  let bitfield = Pers.read_bitfield bf_name bf_len in

  (****** initialize File.t and retrive persistent data *******)

  let file = File.create pieces_hash ~piece_length ~total_length in

  info "read from files: %d pieces" (File.num_owned_pieces file);
  debug "read from files: %s" (File.pieces_to_string file);

  let read_piece i : unit Deferred.t =
      let p = File.get_piece file i in
      Pers.read_piece pers p 
      >>= fun () -> 
      if Piece.is_hash_ok p then 
        return (File.set_piece_status file i `Downloaded)
      else 
        raise (Wrong_piece i)
  in

  let bitset = Bitset.from_bitfield bitfield num_pieces in
  let l = Bitset.to_list bitset in

  Deferred.List.iter l ~f:read_piece 

  >>= fun () ->

  (******* create Pwp.t *************)

  let pwp = Pwp.create t file pers in

  wait_for_incoming_peers pwp t;
  Signal.handle Signal.terminating ~f:(fun _ -> stop bf_name file pers);
  Pwp.start pwp;
  add_peers_from_tracker pwp t addrs







