open Core
open Async
open Log.Global

type t = {
  file : File.t;
  peer : Peer.t;
  this_peer_id : string;
  choked : bool; 
  interested : bool; 
}

let create peer_addr file this_peer_id = 
  Peer.create peer_addr file.File.num_pieces
  >>| function 
  | Ok peer -> Ok { file; peer; this_peer_id; choked = true; interested = true }
  | Error exn -> Error exn

let loop_forever_every_n f param s =
  let rec loop () =
    f param;
    after s 
    >>= fun () -> 
    loop()
  in
  Deferred.don't_wait_for (Deferred.ignore (loop())) 

let request_piece (p:Peer.t) (piece:Piece.t) : unit =
  info "Requesting piece %d from peer %s" piece.Piece.index (Peer.to_string p);
  assert (piece.Piece.status = `Not_requested);
  piece.Piece.status <- `Requested; 
  let blocks = Piece.blocks piece in
  let f (offset, len) =
  let m = Message.Request(Int32.of_int_exn piece.Piece.index, offset, len) in
  (* sexp (Message.sexp_of_t m); *)
  Deferred.don't_wait_for(Peer.send_message p m)
in
  List.iter blocks ~f

(** find first piece not yet requested owned by peer p.
    TODO make this more ocaml-idiomatic! *)
let first_not_requested (f:File.t) (p:Peer.t) : Piece.t option =
    let n = f.File.num_pieces in
    let i = ref 0 in
    let res = ref None in
    while !i < n do
      let piece = f.File.pieces.(!i) in
      if Bitset.get p.Peer.have !i && (piece.Piece.status = `Not_requested)
      then (
        res := Some piece
      );
      incr i
    done;
    !res

let download_pieces x =
  if not x.peer.Peer.choked then (
    debug "%s isn't choked, may ask him some pieces" (Peer.to_string x.peer);
    let piece_opt = first_not_requested x.file x.peer in
    match piece_opt with 
    | Some piece -> request_piece x.peer piece 
    | None -> debug "nothing to download"
  ) else (
    debug "%s is choked, don't send request" (Peer.to_string x.peer) 
  )

let keep_alive x =
  let m  = Message.Interested in
  debug "sending interested message to %s" (Peer.to_string x.peer);
  sexp (Message.sexp_of_t m);
  Peer.send_message x.peer m

(** process all incoming messages *)
let loop_wait_message x = 
  let rec wait_message x =
    let process_message p m =
      let open Message in
      let open Peer in
      match m with
      | KeepAlive -> ()
      | Choke -> p.choked <- true
      | Unchoke -> p.choked <- false
      | Interested -> p.interested <- true
      | Not_interested -> p.interested <- false
      | Have index -> Bitset.set p.have (Int32.to_int_exn index) true 
      | Bitfield bits  -> Bitset.fill_from_string bits p.have
      | Request (index, bgn, length) -> debug "ignore request - not yet implemented"
      | Piece (index, bgn, block) ->  
        let piece = x.file.File.pieces.(Int32.to_int_exn(index)) in
        Piece.update piece bgn block 
      | Cancel (index, bgn, length) -> debug "ignore cancel msg - Not yet implemented"
    in
    Peer.get_message x.peer 
    >>= fun m ->
    debug "got message from %s %s" (Peer.to_string x.peer) (Message.to_string m);
    process_message x.peer m;
    wait_message x
  in
  info "Start message handler loop";
  Deferred.don't_wait_for (Deferred.ignore (wait_message x))

let init x =
  Peer.handshake x.peer x.file.File.sha x.this_peer_id
  >>| function 
  | Ok () ->  
    info "handshake ok with peer %s" (Peer.to_string x.peer);
    loop_wait_message x;
    loop_forever_every_n keep_alive x (sec 120.0);
    loop_forever_every_n download_pieces x (sec 5.0);
    Ok ()
  | Error err -> Error err




