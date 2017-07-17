open Core
open Async
open Log.Global

module M = Message
module P = Peer
module G = Global

type t = {
  info_hash : Bt_hash.t;
  file : File.t;
  mutable peers : P.t list;
  peer_id : Peer_id.t;
}

let create info_hash file peer_id = { 
  file; 
  peers = []; 
  peer_id; 
  info_hash;
}

let num_pending t = 
  List.fold t.peers ~init:0 ~f:(fun acc p -> acc + (P.pending_size p)) 

let for_all_non_idle_peers t ~f =
  let f p = if not (P.is_idle p) then f p in List.iter t.peers ~f 

let cancel_requested_pieces t peer =
  if P.has_pending peer then (
    let f i = Piece.set_status (File.get_piece t.file i) `Not_requested in
    info "Cancelling queries %s" (P.pending_to_string peer) ;
    P.iter_pending peer ~f;
    P.clear_pending peer)

(* notify peers that we have a pieces they don't have *)
let send_have_messages t i =
  let notify_if_doesn't_have i p =
    if not (P.has_piece p i) then (
      info "notify peer %s about piece %d" (P.to_string p) i;
      P.send_message p (M.Have i)
    ) in
  for_all_non_idle_peers t ~f:(notify_if_doesn't_have i)

let tick_peers t = 
  let f p = 
    P.incr_time p;
    if ((P.time_since_last_received_message p) >= 15) then (
      info "Peer %s seems to be idle" (P.to_string p);
      cancel_requested_pieces t p;
      P.set_idle p true;
    )
  in
  for_all_non_idle_peers t ~f

let request_all_blocks_from_piece (p:P.t) (piece:Piece.t) : unit =
  info "Requesting piece %s from peer %s" (Piece.to_string piece) 
    (P.to_string p);
  Piece.set_status piece `Requested;
  P.add_pending p (Piece.get_index piece);
  let f ~index ~off ~len =
    let m = M.Request(index, off, len) in
    sexp ~level:`Debug (M.sexp_of_t m); 
    P.send_message p m in
  Piece.iter piece ~f

let compute_next_request t : (Piece.t * P.t) Option.t =
  let f peer = 
    if (P.is_idle peer) || (P.is_peer_choking peer) || ((num_pending t) >= G.max_pending_request) then
      None
    else 
      (* not very efficient *)
      let pieces_not_requested = File.pieces_not_requested t.file in
      let pieces_owned_by_peer = Peer.owned_pieces peer in
      let pieces_to_request = Bitset.inter pieces_not_requested pieces_owned_by_peer in
      match Bitset.choose_random pieces_to_request with 
      | None -> None 
      | Some i -> Some (File.get_piece t.file i, peer)
  in
  let l = List.map t.peers ~f in
  match List.find l ~f:is_some with
  | None -> None
  | Some x -> x

let request_piece t =
  debug "request piece";
  match compute_next_request t with
  | None -> ()
  | Some (piece, peer) -> request_all_blocks_from_piece peer piece  

let process_message t (p:P.t) (m:M.t) : unit =
  let process_piece_message index bgn block =
    let piece = File.get_piece t.file index in
    match Piece.update piece bgn block with 
    | `Ok -> debug "got block - piece %d offset = %d" index bgn
    | `Hash_error -> debug "hash error for piece %d" index
    | `Downloaded ->
      info "downloaded piece %d" index;
      P.remove_pending p index;
      File.set_owned_piece t.file index; 
      send_have_messages t index 
  in
  let process_request index bgn length =
    Peer.validate p (File.has_piece t.file index);
    assert false (* TODO *)
  in
  match m with
  | M.KeepAlive -> ()
  | M.Choke -> P.set_peer_choking p true
  | M.Unchoke -> P.set_peer_choking p false
  | M.Interested -> 
    P.set_peer_interested p true; 
    (* TODO implement strategy to decide when I am not choking *) 
    if not (P.am_choking p) then
      P.send_message p Message.Unchoke
  | M.Not_interested -> P.set_peer_interested p false
  | M.Have index -> P.set_owned_piece p index 
  | M.Bitfield bits -> P.set_owned_pieces p bits 
  | M.Request (index, bgn, length) -> 
    if not (Peer.am_choking p) then process_request index bgn length
  | M.Piece (index, bgn, block) -> process_piece_message index bgn block 
  | M.Cancel (index, bgn, length) -> info "ignore cancel msg - Not yet implemented"

let rec wait_and_process_message t (p:P.t) =
  P.get_message p 
  >>| function
  | `Ok m -> process_message t p m; `Repeat ()
  | `Eof -> `Finished ()

let stats t =
  info "**** downloaded %d/%d ****" (File.num_owned_pieces t.file) 
    (File.num_pieces t.file);
  let f p = Peer.stats p in
  List.iter t.peers f

let add_peer t peer_addr = 
  let init_protocol (p:P.t) =
    t.peers <- p :: t.peers;
    P.handshake p t.info_hash t.peer_id
    >>> function 
    | Ok () ->  
      debug "handshake ok with peer %s" (P.to_string p);
      if (File.num_owned_pieces t.file) > 0 then (
        P.send_message p (M.Bitfield (File.bitfield t.file))
      );
      P.send_message p M.Interested;
      debug "Start message handler loop";
      Deferred.repeat_until_finished () (fun () -> wait_and_process_message t p)
      >>> fun () -> ()
    | Error err -> info "handshake failed"
  in 

  P.create peer_addr (File.num_pieces t.file)
  >>> function 
  | Ok peer -> init_protocol peer
  | Error err -> 
    let s = Socket.Address.Inet.to_string peer_addr in
    info "can't connect to peer %s" s

let stop t = 
  let stop_aux t =
    info "terminating";
    File.write_to_file_and_close t.file
    >>= fun () -> 
    exit 0
  in 
  don't_wait_for (stop_aux t)

let start t =  
  Clock.every G.tick (fun () -> tick_peers t); 
  Clock.every (sec 0.001) (fun () -> request_piece t)











