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

let loop_forever_every_n f s =
  let rec loop () =
    f ();
    after s 
    >>= fun () -> 
    loop()
  in
  Deferred.don't_wait_for (Deferred.ignore (loop())) 

let request_piece (p:Peer.t) (piece:Piece.t) : unit =
  info "Requesting piece %d (len = %d) from peer %s" 
    (Piece.get_index piece)
    (Piece.length piece)
    (Peer.to_string p);
  Piece.set_requested piece;
  for i = 0 to (Piece.num_blocks piece) - 1 do 
    let (offset, len) = Piece.offset_length piece i in 
    let m = Message.Request(Int32.of_int_exn (Piece.get_index piece), offset, len) in
    sexp ~level:`Debug (Message.sexp_of_t m); 
    Peer.send_message p m
  done

(** find first piece not yet requested owned by peer p. *)
let first_not_requested (file:File.t) (p:Peer.t) : Piece.t option =
  let f piece = 
    Bitset.get p.Peer.have (Piece.get_index piece) && 
    (Piece.to_be_downloaded piece) in
  Array.find file.File.pieces ~f

let download_pieces t peer =
  if not peer.Peer.choked then (
    debug "%s isn't choked, may ask him some pieces" (Peer.to_string peer);
    let piece_opt = first_not_requested t.file peer in
    match piece_opt with 
    | Some piece -> request_piece peer piece 
    | None -> debug "nothing to download"
  ) else (
    debug "%s is choked, don't send request" (Peer.to_string peer) 
  )

let keep_alive peer =
  let m  = Message.Interested in
  debug "sending interested message to %s" (Peer.to_string peer);
  sexp ~level:`Debug (Message.sexp_of_t m);
  Peer.send_message peer m

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
      | Interested -> peer.interested <- true
      | Not_interested -> peer.interested <- false
      | Have index -> Bitset.set peer.have (Int32.to_int_exn index) true 
      | Bitfield bits  -> Bitset.fill_from_string peer.have bits
      | Request (index, bgn, length) -> debug "ignore request - not yet implemented"
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
  info "Start message handler loop";
  Deferred.don't_wait_for (Deferred.ignore (wait_message t))

let add_peer t peer_addr = 
  Peer.create peer_addr t.file.File.num_pieces
  >>= function 
  | Ok peer -> 
    t.peers <- peer :: t.peers;
    Peer.handshake peer t.file.File.hash t.peer_id
    >>| ( function 
        | Ok () ->  
          info "handshake ok with peer %s" (Peer.to_string peer);
          loop_wait_message t peer;
          info "sending Interested message to peer %s" (Peer.to_string peer);
          Peer.send_message peer Message.Interested;
          loop_forever_every_n (fun () -> download_pieces t peer) (sec 1.0);
          Ok ()
        | Error err -> debug "ignore err in add_peer"; Error err)
  | Error err -> debug "ignore err in add_peer"; return (Error err)

let start t peer_addrs = 
  (* TODO what is better to return here? unit or unit Deferred.t,
     something like type never_return? *)
  let silent_add_peer peer_addr : unit =
    Deferred.don't_wait_for (Deferred.ignore (add_peer t peer_addr))
  in return (List.iter ~f:silent_add_peer peer_addrs)
