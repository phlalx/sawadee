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
}

let create t file pers = 
  let peer_id = G.peer_id in
  { file; peers = []; num_requested = 0; pers }

(* TODO make sure this invariant is maintained, maybe move this 
   somewhere else *)
let incr_requested t =
  t.num_requested <- t.num_requested + 1 

let decr_requested t =
  t.num_requested <- t.num_requested - 1

let for_all_peers t ~f = List.iter t.peers ~f

(* notify peers that we have a pieces they don't have *)
let send_have_messages t i =
  let notify_if_doesn't_have i p =
    if not (P.has_piece p i) then (
      debug "notify peer %s about piece %d" (P.to_string p) i;
      P.send_message p (M.Have i)
    ) in
  for_all_peers t ~f:(notify_if_doesn't_have i)

let tick_peers t = 
  let f p = 
    match P.tick p with
    | `Ok -> ()
    | `Idle l -> 
      let f i = 
        decr_requested t;
        Piece.set_status (File.get_piece t.file i) `Not_requested in
      let s = List.to_string ~f:string_of_int l in
      if not (List.is_empty l) then
        info "cancelling requests from %s: %s" (Peer.to_string p) s;
      List.iter l ~f
    | `Keep_alive -> 
      info "sending keep alive to peer %s" (Peer.to_string p);
      Peer.send_message p Message.KeepAlive 
  in
  for_all_peers t ~f

let request_all_blocks_from_piece t (p:P.t) (piece:Piece.t) : unit =
  debug "requesting piece %s from peer %s" (Piece.to_string piece) 
    (P.to_string p);
  incr_requested t;
  Piece.set_status piece `Requested;
  P.add_pending p (Piece.get_index piece);
  let f ~index ~off ~len =
    let m = M.Request(index, off, len) in
    sexp ~level:`Debug (M.sexp_of_t m); 
    P.send_message p m in
  Piece.iter piece ~f

(** this function is polled regularly to see if there's new stuff to download.
    We could instead have an event-loop that wakes up when the pending queue
    is small and new pieces are availables TODO *)
let request_piece t =
  if t.num_requested < G.max_pending_request then
    match Strategy.next_request t.file t.peers with
    | None -> ()
    | Some (piece, peer) -> request_all_blocks_from_piece t peer piece  

let process_message t (p:P.t) (m:M.t) : unit =
  let process_piece_message index bgn block =
    let piece = File.get_piece t.file index in
    match Piece.update piece bgn block with 
    | `Ok -> debug "got block - piece %d offset = %d" index bgn
    | `Hash_error -> 
      decr_requested t;
      Piece.set_status piece `Not_requested;
      info "hash error piece %d from %s" index (P.to_string p)
    | `Downloaded ->
      info "got piece %d from %s " index (P.to_string p);
      P.remove_pending p index;
      Piece.set_status piece `Downloaded;
      decr_requested t;
      (* Piece.set_status piece `On_disk;  TODO needed? *)
      Pers.write_piece t.pers piece;
      File.set_owned_piece t.file index; 
      send_have_messages t index 
  in
  let process_request index bgn length =
    Peer.validate p (File.has_piece t.file index);
    if not (Peer.am_choking p) then (
      let piece = File.get_piece t.file index in
      let block = Piece.get_content piece ~off:bgn ~len:length in
      Peer.send_message p (Message.Piece (index, bgn, block)))
  in
  match m with
  | M.KeepAlive -> ()
  | M.Choke -> P.set_peer_choking p true
  | M.Unchoke -> P.set_peer_choking p false
  | M.Interested -> 
    P.set_peer_interested p true; 
    if not (P.am_choking p) then P.send_message p Message.Unchoke
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
  info "** stats:";
  info "** downloaded %d/%d" (File.num_owned_pieces t.file) 
    (File.num_pieces t.file);
  info "** pending requests %d" t.num_requested;
  let f p = Peer.stats p in
  List.iter t.peers f

let add_peer t p =
  t.peers <- p :: t.peers;
  if (File.num_owned_pieces t.file) > 0 then (
    P.send_message p (M.Bitfield (File.bitfield t.file))
  );
  P.send_message p M.Interested;
  debug "start message handler loop";
  Deferred.repeat_until_finished () (fun () -> wait_and_process_message t p)

let start t =  
  Clock.every (sec 10.0) (fun () -> stats t); 
  Clock.every G.tick (fun () -> tick_peers t); 
  Clock.every (sec 0.1) (fun () -> request_piece t)



