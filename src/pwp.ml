open Core
open Async
open Log.Global

module M = Message
module P = Peer
module S = State
module G = Global

type peer_info = {
  state : S.t;
  peer : P.t;
}

let peer_id pi = P.id pi.peer

let to_string pi = peer_id pi |> Peer_id.to_readable_string 

type t = {
  file : File.t;
  pers : Pers.t;
  mutable num_requested : int;
  mutable torrent : Torrent.t;
  peers : (Peer_id.t, peer_info) Hashtbl.t 
}

let torrent t = t.torrent

let create torrent file pers = { file; peers = Hashtbl.Poly.create ();
                                 num_requested = 0; pers; torrent }

(* This has to be called whenever a request is sent *)
let incr_requested t = t.num_requested <- t.num_requested + 1 

(* This has to be called whenever a request has been granted or cancelled *)
let decr_requested t = t.num_requested <- t.num_requested - 1

let for_all_peers t ~f = Hashtbl.iter t.peers ~f

let send_have_messages t i =
  let notify_if_doesn't_have i pi =
    if not (S.has_piece pi.state i) then (
      debug "notify peer %s about piece %d" (to_string pi) i;
      P.send_message pi.peer (M.Have i)
    ) in
  for_all_peers t ~f:(notify_if_doesn't_have i)

(* we always request all blocks from a piece to the same peer at the 
   same time *)
let request_all_blocks_from_piece t (pi:peer_info) (piece_i:int) : unit =
  debug "requesting piece %d from peer %s" piece_i (to_string pi);
  incr_requested t;
  File.set_piece_status t.file piece_i `Requested; 
  S.add_pending pi.state piece_i;
  let f ~index ~off ~len =
    let m = M.Request(index, off, len) in
    sexp ~level:`Debug (M.sexp_of_t m); 
    P.send_message pi.peer m in
  Piece.iter (File.get_piece t.file piece_i) ~f

(* try to request as many pieces as we can - there should be no more than
   G.max_pending_request *)
let try_request_pieces t =
  let n = G.max_pending_request - t.num_requested in
  if n > 0 then 

    let f pi = pi.peer, pi.state in  
    let new_table = Hashtbl.map t.peers ~f in
    let l = Strategy.next_requests t.file new_table n in
    let f (piece_i, (peer, state)) = 
      let pi = { peer; state } in 
      request_all_blocks_from_piece t pi piece_i 
    in
    List.iter l ~f

let process_message t (pi:peer_info) (m:M.t) : unit =

  let process_block index bgn block =
    let piece = File.get_piece t.file index in
    let len = String.length block in
    Peer.validate pi.peer (File.is_valid_piece_index t.file index);
    Peer.validate pi.peer (Piece.is_valid_block piece bgn len);
    match Piece.update piece bgn block with 
    | `Ok -> debug "got block - piece %d offset = %d" index bgn
    | `Hash_error -> 
      decr_requested t;
      File.set_piece_status t.file index `Not_requested;
      info "hash error piece %d from %s" index (to_string pi)
    | `Downloaded ->
      debug "got piece %d from %s " index (to_string pi);
      Peer.set_downloading pi.peer;
      State.remove_pending pi.state index;
      File.set_piece_status t.file index `Downloaded;
      decr_requested t;
      Pers.write_piece t.pers piece;

      let one_percent = max (t.torrent.Torrent.num_pieces / 100) 1 in
      if (File.num_downloaded_pieces t.file % one_percent) = 0 then
        Print.printf "downloaded %d%%\n" (File.percent t.file);

      (* notify peers that we have a piece they don't have. 
         TODO: we should do it too if we receive a bitfield *)
      send_have_messages t index 
  in

  let process_extended id b = 
    info "received extended message id = %d" id;
    let bc = Bencode_ext.decode (`String b) in
    info "%s" (Bencode_ext.pretty_print bc)
  in

  let process_request index bgn length =
    let piece = File.get_piece t.file index in
    P.validate pi.peer (File.is_valid_piece_index t.file index);
    P.validate pi.peer (Piece.is_valid_block_request piece bgn length);
    P.validate pi.peer (File.has_piece t.file index);
    if not (State.am_choking pi.state) then (
      P.set_uploading pi.peer;
      let piece = File.get_piece t.file index in
      (* TODO: we could avoid a string allocation by using a substring 
         for the block in M.Piece *)
      let block = Piece.get_content piece ~off:bgn ~len:length in
      P.send_message pi.peer (Message.Piece (index, bgn, block)))
  in
  match m with
  | M.KeepAlive -> ()
  | M.Choke -> S.set_peer_choking pi.state true;
  | M.Unchoke -> 
    S.set_peer_choking pi.state false; 
    (* we try to request new pieces after any new event that can trigger
       availability of new pieces *)
    try_request_pieces t
  | M.Interested -> 
    S.set_peer_interested pi.state true; 
    if not (S.am_choking pi.state) then P.send_message pi.peer Message.Unchoke
  | M.Not_interested -> 
    S.set_peer_interested pi.state false;
  | M.Have index -> 
    S.set_owned_piece pi.state index; 
    try_request_pieces t 
  | M.Bitfield bits -> 
    (* TODO validate bitfield. Not a big deal, but extra bits of the bitfield
       should be set to 0 *)
    S.set_owned_pieces pi.state bits; 
    try_request_pieces t 
  | M.Request (index, bgn, length) -> 
    if not (S.am_choking pi.state) then process_request index bgn length
  | M.Piece (index, bgn, block) -> 
    (* the spec calls this message a piece when it really is a block of a piece *) 
    process_block index bgn block; 
    try_request_pieces t
  | M.Cancel (index, bgn, length) ->
    info "ignore cancel msg - Not yet implemented"
  | M.Port port -> 
    if G.is_node () then (
      Addr.create (Peer.addr pi.peer) port |> 
      Krpc.try_add |> Deferred.ignore |> don't_wait_for )
  | M.Extended (id, b) -> 
    process_extended id b 



let cancel_requests t (pi:peer_info) = 
  let f i = 
    decr_requested t;
    State.remove_pending pi.state i;
    File.set_piece_status t.file i `Not_requested 
  in
  let l = State.get_pending pi.state in 
  if not (List.is_empty l) then (
    let s = List.to_string ~f:string_of_int l in 
    info "cancelling requests from %s: %s" (to_string pi) s
  );
  List.iter l ~f

let remove_peer t pid : unit = 
  (* we can safely remove it, as we knows the connection has been cut. 
     TODO: is the fd properly disposed of? *)
  Hashtbl.remove t.peers pid

(* This is the main message processing loop. We consider two types of events.
   Timeout (idle peer), and message reception. *)
let rec wait_and_process_message t (pi:peer_info) =

  let result = function
    | `Ok m -> 
      process_message t pi m; 
      return (`Repeat ())
    | `Eof ->  
      (* signal the deconnection of the peer *)
      cancel_requests t pi;
      info "peer %s has left - remove it from peers" (to_string pi); 
      remove_peer t (peer_id pi);
      return (`Finished ())

  in
  Clock.with_timeout G.idle (P.get_message pi.peer)  
  >>= function
  | `Timeout -> 
    (* TODO decide what to do with these idle peers - keep using them but
       mark them as bad and give priority to other peers? now we just ignore
       them. *)
    info "peer %s is slow - set idle" (to_string pi); 
    cancel_requests t pi;
    State.set_idle pi.state true;
    return (`Finished ())
  | `Result r -> result r

let initiate_protocol t pi : unit Deferred.t =

  let p = pi.peer in

  (* we send this optional message if we own pieces of the file *)
  if (File.num_downloaded_pieces t.file) > 0 then (
    info "sending my bitfield to %s" (Peer.to_string p);
    P.send_message p (M.Bitfield (File.bitfield t.file))
  );
  (* this should only be sent to peers we're interested in. To simplify, 
     we suppose we're intersted in all peers, but it should be changed TODO *)
  P.send_message p M.Interested;

  info "start message handler loop";
  Deferred.repeat_until_finished () (fun () -> wait_and_process_message t pi)

let add_peer t peer =
  (* we ignore all peers already connected, and ourselves. It may be the case
     that the calling layers try to add twice the same peer. For instance,
     the tracker can return our own address and we may try to connect to 
     ourselves  *)

  let peer_id = Peer.id peer in
  let state = State.create () in
  State.init_size_owned_pieces state t.torrent.Torrent.num_pieces;
  let pi = { peer; state } in

  match Hashtbl.add t.peers ~key:peer_id ~data:pi with 
  | `Ok  -> initiate_protocol t pi |> Deferred.ok
  | `Duplicate -> return (Error (Error.of_string "ignore peers (already added or ourselves)")) 


