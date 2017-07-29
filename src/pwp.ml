open Core
open Async
open Log.Global

module M = Message
module P = Peer
module G = Global

type t = {
  file : File.t;
  mutable peers : P.t list;
  pers : Pers.t;
  mutable num_requested : int;
  mutable torrent : Torrent.t;
}

let create torrent file pers = { file; peers = []; num_requested = 0; pers;
                                 torrent }

(* This has to be called whenever a request is sent *)
let incr_requested t = t.num_requested <- t.num_requested + 1 

(* This has to be called whenever a request has been granted or cancelled *)
let decr_requested t = t.num_requested <- t.num_requested - 1

let for_all_peers t ~f = List.iter t.peers ~f

let send_have_messages t i =
  let notify_if_doesn't_have i p =
    if not (P.has_piece p i) then (
      debug "notify peer %s about piece %d" (P.to_string p) i;
      P.send_message p (M.Have i)
    ) in
  for_all_peers t ~f:(notify_if_doesn't_have i)

(* we always request all blocks from a piece to the same peer at the 
   same time *)
let request_all_blocks_from_piece t (p:P.t) (piece_i:int) : unit =
  debug "requesting piece %d from peer %s" piece_i (P.to_string p);
  incr_requested t;
  File.set_piece_status t.file piece_i `Requested; 
  P.add_pending p piece_i;
  let f ~index ~off ~len =
    let m = M.Request(index, off, len) in
    sexp ~level:`Debug (M.sexp_of_t m); 
    P.send_message p m in
  Piece.iter (File.get_piece t.file piece_i) ~f

(* try to request as many pieces as we can - there should be no more than
   G.max_pending_request *)
let try_request_pieces t =
  let n = G.max_pending_request - t.num_requested in
  if n > 0 then 
    let l = Strategy.next_requests t.file t.peers n in
    let f (piece_i, peer) = request_all_blocks_from_piece t peer piece_i in
    List.iter l ~f

let process_message t (p:P.t) (m:M.t) : unit =

  let process_block index bgn block =
    let piece = File.get_piece t.file index in
    let len = String.length block in
    Peer.validate p (File.is_valid_piece_index t.file index);
    Peer.validate p (Piece.is_valid_block piece bgn len);
    match Piece.update piece bgn block with 
    | `Ok -> debug "got block - piece %d offset = %d" index bgn
    | `Hash_error -> 
      decr_requested t;
      File.set_piece_status t.file index `Not_requested;
      info "hash error piece %d from %s" index (P.to_string p)
    | `Downloaded ->
      info "got piece %d from %s " index (P.to_string p);
      Peer.set_downloading p;
      P.remove_pending p index;
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

  let process_request index bgn length =
    let piece = File.get_piece t.file index in
    Peer.validate p (File.is_valid_piece_index t.file index);
    Peer.validate p (Piece.is_valid_block_request piece bgn length);
    Peer.validate p (File.has_piece t.file index);
    if not (Peer.am_choking p) then (
      Peer.set_uploading p;
      let piece = File.get_piece t.file index in
      (* TODO: we could avoid a string allocation by using a substring 
         for the block in M.Piece *)
      let block = Piece.get_content piece ~off:bgn ~len:length in
      Peer.send_message p (Message.Piece (index, bgn, block)))
  in
  match m with
  | M.KeepAlive -> ()
  | M.Choke -> P.set_peer_choking p true;
  | M.Unchoke -> 
    P.set_peer_choking p false; 
    (* we try to request new pieces after any new event that can trigger
       availability of new pieces *)
    try_request_pieces t
  | M.Interested -> 
    P.set_peer_interested p true; 
    if not (P.am_choking p) then P.send_message p Message.Unchoke
  | M.Not_interested -> 
    P.set_peer_interested p false;
  | M.Have index -> 
    P.set_owned_piece p index; 
    try_request_pieces t 
  | M.Bitfield bits -> 
    (* TODO validate bitfield. Not a big deal, but extra bits of the bitfield
       should be set to 0 *)
    P.set_owned_pieces p bits; 
    try_request_pieces t 
  | M.Request (index, bgn, length) -> 
    if not (Peer.am_choking p) then process_request index bgn length
  | M.Piece (index, bgn, block) -> 
    (* the spec calls this message a piece when it really is a block of a piece *) 
    process_block index bgn block; 
    try_request_pieces t
  | M.Cancel (index, bgn, length) ->
    info "ignore cancel msg - Not yet implemented"
  | M.Port port -> Krpc.try_add (Peer.addr p) port

let cancel_requests t p = 
  let f i = 
    decr_requested t;
    Peer.remove_pending p i;
    File.set_piece_status t.file i `Not_requested 
  in
  let l = Peer.get_pending p in 
  if not (List.is_empty l) then (
    let s = List.to_string ~f:string_of_int l in 
    info "cancelling requests from %s: %s" (Peer.to_string p) s
  );
  List.iter l ~f

let remove_peer t p : unit = 
  (* we can safely remove it, as we knows the connection has been cut. 
     TODO: is the fd properly disposed of? *)
  t.peers <- List.filter t.peers ~f:(Peer.equals p)

(* This is the main message processing loop. We consider two types of events.
   Timeout (idle peer), and message reception. *)
let rec wait_and_process_message t (p:P.t) =

  let result = function
    | `Ok m -> 
      process_message t p m; 
      return (`Repeat ())
    | `Eof ->  
      (* signal the deconnection of the peer *)
      cancel_requests t p;
      info "peer %s has left - remove it from peers" (Peer.to_string p); 
      remove_peer t p;
      return (`Finished ())

  in
  Clock.with_timeout G.idle (P.get_message p)  
  >>= function
  | `Timeout -> 
    (* TODO decide what to do with these idle peers - keep using them but
       mark them as bad and give priority to other peers? now we just ignore
       them. *)
    info "peer %s is slow - set idle" (Peer.to_string p); 
    cancel_requests t p;
    Peer.set_idle p true;
    return (`Finished ())
  | `Result r -> result r

(* display stats, to be schedule regularly. Not called and not really useful 
   in this form. Keeping it around nonetheless. *)
let stats t =
  info "** stats %d/%d" (File.num_downloaded_pieces t.file) 
    (File.num_pieces t.file);
  info "** pending requests %d" t.num_requested;
  let f p = Peer.stats p in
  List.iter t.peers f

let really_add_peer t p : unit Deferred.t =
  t.peers <- p :: t.peers;

  (* we send this optional message if we own pieces of the file *)
  if (File.num_downloaded_pieces t.file) > 0 then (
    info "sending my bitfield to %s" (Peer.to_string p);
    P.send_message p (M.Bitfield (File.bitfield t.file))
  );
  (* this should only be sent to peers we're interested in. To simplify, 
     we suppose we're intersted in all peers, but it should be changed TODO *)
  P.send_message p M.Interested;

  info "start message handler loop";
  Deferred.repeat_until_finished () (fun () -> wait_and_process_message t p)

let add_peer t p =
  (* we ignore all peers already connected, and ourselves. It may be the case
     that the calling layers try to add twice the same peer. For instance,
     the tracker can return our own address and we may try to connect to 
     ourselves  *)

  let ignored_peers = G.peer_id :: List.map t.peers ~f:Peer.peer_id in
  match List.mem ignored_peers (Peer.peer_id p) (=) with
  | true -> 
    return (Error (Error.of_string "ignore peers (already added or ourselves)")) 
  | false -> 
    really_add_peer t p |> Deferred.ok



