open Core
open Async
open Log.Global

module M = Message
module P = Peer

type t = {
  file : File.t;
  mutable peers : P.t list;
  peer_id : string;
  choked : bool; 
  interested : bool; 
}

let create file ~peer_id = 
  { file; 
    peers = []; 
    peer_id; 
    choked = true;
    interested = true }

let for_all_non_idle_peers t ~f =
  let f p = if not (P.is_idle p) then f p in List.iter t.peers ~f 

let cancel_requested_pieces t peer =
  if not (Int.Set.is_empty peer.P.pending) then (
    let f i = Piece.set_status (File.get_piece t.file i) `Not_requested in
    info "Cancelling queries %s" (Peer.pending_to_string peer) ;
    Int.Set.iter peer.P.pending ~f;
    peer.P.pending <- Int.Set.empty )

(* notify peers that we have a pieces they don't have *)
let send_have_messages t i =
  let notify_if_doesn't_have i p =
    if not (P.has_piece p i) && (P.is_interested p) then (
      info "notify peer %s about piece %d" (P.to_string p) i;
      P.send_message p (Message.Have i)
    ) in
  List.iter t.peers ~f:(notify_if_doesn't_have i)

let tick_peers t = 
  let f p = 
    P.incr_time p;
    if (p.P.time_since_last_reception >= 15) then (
      info "Peer %s seems to be idle" (P.to_string p);
      cancel_requested_pieces t p;
      p.P.idle <- true;
    )
  in
  for_all_non_idle_peers t ~f

let request_all_blocks_from_piece (p:P.t) (piece:Piece.t) : unit =
  info "Requesting piece %s from peer %s" (Piece.to_string piece) 
    (P.to_string p);
  Piece.set_status piece `Requested;
  p.P.pending <- Int.Set.add p.P.pending (Piece.get_index piece);
  let f ~index ~off ~len =
    let m = Message.Request(index, off, len) in
    sexp ~level:`Debug (Message.sexp_of_t m); 
    P.send_message p m in
  Piece.iter piece ~f

let compute_next_request t : (Piece.t * P.t) Option.t =
  let f peer = 
    if peer.P.idle || peer.P.choked || Int.Set.length peer.P.pending >= 3 then
      None
    else 
      let open Bitset in
      let f i = (Piece.get_status (File.get_piece t.file i) = `Not_requested) in
      let pieces_to_be_downloaded = Bitset.init (File.num_pieces t.file) ~f in
      match choose (peer.P.have & pieces_to_be_downloaded) with
      | None -> None 
      | Some (i) -> Some (File.get_piece t.file i, peer)
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

let process_message t (p:Peer.t) (m:Message.t) : unit =
  match m with
  | M.KeepAlive -> ()
  | M.Choke -> p.P.choked <- true
  | M.Unchoke -> p.P.choked <- false
  | M.Interested -> 
    p.P.interested <- true; 
    info "ignore request - not yet implemented"
  | M.Not_interested -> p.P.interested <- false
  | M.Have index -> Bitset.set p.P.have index true 
  | M.Bitfield bits  -> 
    Bitset.fill_from_string p.P.have bits;
    info "Peer %s has %d/%d pieces" (P.to_string p) 
      (Bitset.num_bit_set p.P.have) 
      (File.num_pieces t.file)
  | M.Request (index, bgn, length) -> 
    info "ignore request - not yet implemented"
  | M.Piece (index, bgn, block) -> (
      let piece = File.get_piece t.file index in
      let len = String.length block in
      debug "got piece %d begin = %d len = %d" index bgn len;
      match Piece.update piece bgn block with 
      | `Ok -> () 
      | `Hash_error -> debug "hash error"
      | `Downloaded ->
        p.P.pending <- Int.Set.remove p.P.pending index;
        File.set_piece_have t.file index; 
        send_have_messages t index; 
        info "downloaded piece %d" index)
  | M.Cancel (index, bgn, length) -> 
    debug "ignore cancel msg - Not yet implemented"

let rec wait_and_process_message t (p:Peer.t) =
  Peer.get_message p 
  >>| function
  | `Ok m -> process_message t p m; `Repeat ()
  | `Eof -> `Finished ()

let display_downloaded t =
  info "**** downloaded %d/%d ****" 
    (File.num_piece_have t.file)
    (File.num_pieces t.file)
(*; File.write_to_disk t.file *) (* TODO: debug this *)

let add_peer t peer_addr = 
  let init_protocol (p:Peer.t) =
    t.peers <- p :: t.peers;
    P.handshake p (File.hash t.file) t.peer_id
    >>= function 
    | Ok () ->  
      debug "handshake ok with peer %s" (P.to_string p);
      if (File.num_piece_have t.file) > 0 then (
        P.send_message p (Message.Bitfield (File.bitset t.file))
      );
      P.send_message p Message.Interested;
      debug "Start message handler loop";
      Deferred.repeat_until_finished () (fun () -> wait_and_process_message t p)
    | Error err -> info "handshake failed"; return () in 

  let add_peer_aux t peer_addr = 
    Peer.create peer_addr (File.num_pieces t.file)
    >>= function 
    | Ok peer -> init_protocol peer
    | Error err -> info "Can't connect to peer"; return () in

  Deferred.don't_wait_for (add_peer_aux t peer_addr)

let start t = 
  Clock.every (sec 10.0) (fun () -> display_downloaded t); 
  Clock.every (sec 1.0) (fun () -> tick_peers t); 
  Clock.every (sec 0.001) (fun () -> request_piece t)


