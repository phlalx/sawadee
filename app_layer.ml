open Core
open Async
open Log.Global

type t = {
  file : File.t;
  mutable peer : Peer.t list; (* TODO is this needed? *)
  this_peer_id : string;
  choked : bool; 
  interested : bool; 
}

let create file this_peer_id = { file; peer = []; this_peer_id; choked = true;
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
  info "Requesting piece %d from peer %s" (Piece.get_index piece) (Peer.to_string p);
  Piece.set_requested piece;
  let blocks = Piece.blocks piece in
  let f (offset, len) =
    let m = Message.Request(Int32.of_int_exn (Piece.get_index piece), offset, len) in
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
    if Bitset.get p.Peer.have !i && (Piece.to_be_downloaded piece)
    then (
      res := Some piece
    );
    incr i
  done;
  !res

let download_pieces x peer =
  if not peer.Peer.choked then (
    debug "%s isn't choked, may ask him some pieces" (Peer.to_string peer);
    let piece_opt = first_not_requested x.file peer in
    match piece_opt with 
    | Some piece -> request_piece peer piece 
    | None -> debug "nothing to download"
  ) else (
    debug "%s is choked, don't send request" (Peer.to_string peer) 
  )

let keep_alive x peer =
  let m  = Message.Interested in
  debug "sending interested message to %s" (Peer.to_string peer);
  sexp (Message.sexp_of_t m);
  Peer.send_message peer m

(** process all incoming messages *)
let loop_wait_message x peer : unit = 
  let rec wait_message x =
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
      | Bitfield bits  -> Bitset.fill_from_string bits peer.have
      | Request (index, bgn, length) -> debug "ignore request - not yet implemented"
      | Piece (index, bgn, block) -> (
          let piece = x.file.File.pieces.(Int32.to_int_exn(index)) in
          let bgn_int = Int32.to_int_exn bgn in
          let len = String.length block in
          debug "got piece begin = %d len = %d" bgn_int len;
          match Piece.update piece bgn block with 
          | `Ok -> 
            debug "got some block!"
          | `Sha_error -> 
            debug "sha error"
          | `Downloaded ->
            x.file.File.pieces_downloaded <- x.file.File.pieces_downloaded + 1;
            info "Downloaded %d/%d pieces" x.file.File.pieces_downloaded 
              x.file.File.num_pieces) 
      | Cancel (index, bgn, length) -> debug "ignore cancel msg - Not yet implemented"
    in
    Peer.get_message peer 
    >>= fun m ->
    debug "got message from %s %s" (Peer.to_string peer) (Message.to_string m);
    process_message m;
    wait_message x
  in
  info "Start message handler loop";
  Deferred.don't_wait_for (Deferred.ignore (wait_message x))

let add_peer al peer_addr = 
  Peer.create peer_addr al.file.File.num_pieces
  >>= function 
  | Ok peer -> 
    al.peer <- peer :: al.peer;
    Peer.handshake peer al.file.File.sha al.this_peer_id
    >>| ( function 
        | Ok () ->  
          info "handshake ok with peer %s" (Peer.to_string peer);
          loop_wait_message al peer;
          loop_forever_every_n (fun () -> keep_alive al peer)  (sec 120.0);
          loop_forever_every_n (fun () -> download_pieces al peer) (sec 1.0);
          Ok ()
        | Error err -> debug "ignore err in add_peer"; Error err)
  | Error err -> debug "ignore err in add_peer"; return (Error err)

let start al peer_addrs = 
  let silent_add_peer peer_addr : unit =
    Deferred.don't_wait_for (Deferred.ignore (add_peer al peer_addr))
    (* TODO better to return unit or unit Deferred.t? *)
  in return (List.iter ~f:silent_add_peer peer_addrs)












