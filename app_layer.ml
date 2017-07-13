open Core
open Async
open Log.Global

type t = {
  file : File.t;
  mutable peers : Peer.t list;
  peer_id : string;
  choked : bool; 
  interested : bool; 
}

let create file ~peer_id = { file; peers = []; peer_id; choked = true;
                             interested = true }


let cancel_requested_pieces t peer =
  let f i = Piece.set_not_requested t.file.File.pieces.(i) in
  Int.Set.iter peer.Peer.pending ~f


(* TODO see Async.Time for an existing function *)
let loop_forever_every_n f s =
  let rec loop () =
    f ();
    after s 
    >>= fun () -> 
    loop()
  in
  Deferred.don't_wait_for (Deferred.ignore (loop())) 

let request_all_blocks_from_piece (p:Peer.t) (piece:Piece.t) : unit =
  info "Requesting piece %d (len = %d) from peer %s" 
    (Piece.get_index piece)
    (Piece.length piece)
    (Peer.to_string p);
  Piece.set_requested piece;
  p.Peer.pending <- Int.Set.add p.Peer.pending (Piece.get_index piece);
  for i = 0 to (Piece.num_blocks piece) - 1 do 
    let (offset, len) = Piece.offset_length piece i in 
    let m = Message.Request(Int32.of_int_exn (Piece.get_index piece), offset, len) in
    sexp ~level:`Debug (Message.sexp_of_t m); 
    Peer.send_message p m
  done

let compute_next_request t : (Piece.t * Peer.t) Option.t =
  let f peer = 
    if peer.Peer.choked || Int.Set.length peer.Peer.pending >= 3 then
      None
    else 
      let open Bitset in
      let f i = Piece.to_be_downloaded (t.file.File.pieces.(i)) in
      let pieces_to_be_downloaded = Bitset.init t.file.File.num_pieces ~f in
      match choose (peer.Peer.have & pieces_to_be_downloaded) with
      | None -> None 
      | Some (i) -> Some (t.file.File.pieces.(i), peer)
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

(** process all incoming messages *)
let loop_wait_message t peer : unit = 
  let rec wait_message t =
    let process_message m =
      let open Peer in
      let open Message in
      match m with
      | KeepAlive -> ()
      | Choke -> peer.choked <- true
      | Unchoke -> peer.choked <- false
      | Interested -> peer.interested <- true; info "ignore request - not yet implemented"
      | Not_interested -> peer.interested <- false
      | Have index -> Bitset.set peer.have (Int32.to_int_exn index) true 
      | Bitfield bits  -> 
          Bitset.fill_from_string peer.have bits;
          info "Peer %s has %d/%d pieces" (Peer.to_string peer) (Bitset.num_bit_set peer.have) (t.file.File.num_pieces)
      | Request (index, bgn, length) -> info "ignore request - not yet implemented"
      | Piece (index, bgn, block) -> (
          let index_int = Int32.to_int_exn index in
          let piece = t.file.File.pieces.(index_int) in
          let len = String.length block in
          debug "got piece %ld begin = %ld len = %d" index bgn len;
          match Piece.update piece (Piece.offset_to_index bgn) block with 
          | `Ok -> () 
          | `Hash_error -> 
            info "hash error"
          | `Downloaded ->
            peer.Peer.pending <- Int.Set.remove peer.Peer.pending index_int;
            Bitset.set t.file.File.bitset index_int true; 
            info "downloaded piece %d" index_int)
      | Cancel (index, bgn, length) -> debug "ignore cancel msg - Not yet implemented"
    in
    Peer.get_message peer 
    >>= function
    | `Ok m -> 
      debug "got message from %s %s" (Peer.to_string peer) (Message.to_string m);
      process_message m;
      wait_message t
    | `Eof -> 
      info "Didn't get message - peer %s closed connection" (Peer.to_string peer);
      return ()
  in
  debug "Start message handler loop";
  Deferred.don't_wait_for (Deferred.ignore (wait_message t))

let display_downloaded t =
  let bs = t.file.File.bitset in
  info "**** downloaded %d/%d ****" (Bitset.num_bit_set bs) (t.file.File.num_pieces)

let add_peer t peer_addr = 
  Peer.create peer_addr t.file.File.num_pieces
  >>= function 
  | Ok peer -> 
    t.peers <- peer :: t.peers;
    Peer.handshake peer t.file.File.hash t.peer_id
    >>= ( function 
        | Ok () ->  
          debug "handshake ok with peer %s" (Peer.to_string peer);
          loop_wait_message t peer;
          let bs = t.file.File.bitset in 
          if not (Bitset.is_zero bs) then (
            info "sending bitfield message to peer %s" (Peer.to_string peer); 
            Peer.send_message peer (Message.Bitfield (Bitset.to_string bs))
          );
          debug "sending Interested message to peer %s" (Peer.to_string peer); 
          Peer.send_message peer Message.Interested;
          loop_forever_every_n (fun () -> request_piece t) (sec 1.0); (* TODO should not be here! *)
          return (Ok ())
        | Error err -> info "ignore err in add_peer"; return (Error err))
  | Error err -> info "ignore err in add_peer"; return (Error err)

(* TODO: better to return type never_return? *)
let start t peer_addrs = 
  let silent_add_peer peer_addr : unit =
    Deferred.don't_wait_for (Deferred.ignore (add_peer t peer_addr)) in

  loop_forever_every_n (fun () -> display_downloaded t) (sec 10.0); 
  List.iter ~f:silent_add_peer peer_addrs





