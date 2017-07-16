open Core

open Async
open Log.Global

module M = Message
module P = Peer

type t = {
  file : File.t;
  mutable peers : P.t list;
  peer_id : Peer_id.t;
  choked : bool; 
  interested : bool; 
}

let create file peer_id = { 
  file; 
  peers = []; 
  peer_id; 
  choked = true;
  interested = true
}

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
    if not (P.has_piece p i) && (P.is_interested p) then (
      info "notify peer %s about piece %d" (P.to_string p) i;
      P.send_message p (M.Have i)
    ) in
  List.iter t.peers ~f:(notify_if_doesn't_have i)

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
  let f ~index ~off ~len ~content =
    let m = M.Request(index, off, len) in
    sexp ~level:`Debug (M.sexp_of_t m); 
    P.send_message p m in
  Piece.iter piece ~f

let compute_next_request t : (Piece.t * P.t) Option.t =
  let f peer = 
    if (P.is_idle peer) || (P.is_choking peer) || ((P.pending_size peer) >= 3) then
      None
    else 
      (* TODO this is not efficient *)
      let pieces_not_requested = File.pieces_not_requested t.file in
      let pieces_owned_by_peer = Peer.owned_pieces peer in
      let pieces_to_request = Bitset.inter pieces_not_requested pieces_owned_by_peer in
      match Bitset.choose pieces_to_request with 
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
    let piece = File.get_piece t.file index in
    let f ~index ~off ~len ~content = 
      let m = M.Piece (index, off, content) in
      P.send_message p m 
    in 
    if not t.choked then
      Piece.iter piece ~f
  in
  match m with
  | M.KeepAlive -> ()
  | M.Choke -> P.set_choking p true
  | M.Unchoke -> P.set_choking p false
  | M.Interested -> 
      P.set_interested p true; 
      info "ignore request - not yet implemented"
  | M.Not_interested -> P.set_interested p false
  | M.Have index -> P.set_owned_piece p index 
  | M.Bitfield bits  -> P.set_owned_pieces p bits 
  | M.Request (index, bgn, length) -> process_request index bgn length
  | M.Piece (index, bgn, block) -> process_piece_message index bgn block 
  | M.Cancel (index, bgn, length) -> info "ignore cancel msg - Not yet implemented"

let rec wait_and_process_message t (p:P.t) =
  P.get_message p 
  >>| function
  | `Ok m -> process_message t p m; `Repeat ()
  | `Eof -> `Finished ()

let display_downloaded t =
  info "**** downloaded %d/%d ****" (File.num_owned_pieces t.file) 
    (File.num_pieces t.file)

let add_peer t peer_addr = 
  let init_protocol (p:P.t) =
    t.peers <- p :: t.peers;
    P.handshake p (File.hash t.file) t.peer_id
    >>= function 
    | Ok () ->  
      debug "handshake ok with peer %s" (P.to_string p);
      if (File.num_owned_pieces t.file) > 0 then (
        P.send_message p (M.Bitfield (File.bitfield t.file))
      );
      P.send_message p M.Interested;
      debug "Start message handler loop";
      Deferred.repeat_until_finished () (fun () -> wait_and_process_message t p)
    | Error err -> info "handshake failed"; return () in 

  let add_peer_aux t peer_addr = 
    P.create peer_addr (File.num_pieces t.file)
    >>= function 
    | Ok peer -> init_protocol peer
    | Error err -> info "Can't connect to peer"; return () in

  Deferred.don't_wait_for (add_peer_aux t peer_addr)

let stop t = 
  let stop_aux t =
    info "terminating";
    File.write t.file
    >>= fun () ->
    File.close t.file
    >>= fun () -> 
    exit 0
  in 
  don't_wait_for (stop_aux t)

let start t = 
  Clock.every (sec 10.0) (fun () -> display_downloaded t); 
  Clock.every (sec 1.0) (fun () -> tick_peers t); 
  Clock.every (sec 0.001) (fun () -> request_piece t)


