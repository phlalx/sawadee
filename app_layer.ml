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

let download_pieces x =
  let download_pieces_from_peer (p:Peer.t) (f:File.t) =
    let n = Array.length x.file.File.pieces_downloaded in  
    let i = ref 0 in
    let request_sent = ref 0 in
    let max_request = 5 in (* max num of request to a given peer *)
    while !i < n && !request_sent < max_request do
      if Bitset.get p.Peer.have !i && not f.File.pieces_downloaded.(!i)
      then (
        let piece_length = 1024l in
        let piece_offset = 0l in
        let piece_index = Int32.of_int_exn(!i) in 
        let m  = Message.Request(piece_index,piece_offset,piece_length) in
        debug "Requesting piece %d from peer %s" !i (Peer.to_string p);
        sexp (Message.sexp_of_t m);
        Deferred.don't_wait_for(Peer.send_message p m);
        incr request_sent
      );
      incr i
    done
  in
  let p = x.peer in 
  let f = x.file in
  if not p.Peer.choked then (
    debug "%s isn't choked, may ask him some pieces" (Peer.to_string x.peer);
    download_pieces_from_peer p f
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
      | Request (index, bgn, length) -> ()
      | Piece (index, bgn, block) -> 
        x.file.File.pieces_downloaded.(Int32.to_int_exn(index)) <- true;
      | Cancel (index, bgn, length) -> ()
    in
    Peer.get_message x.peer 
    >>= fun m ->
    debug "got message from %s" (Peer.to_string x.peer);
    process_message x.peer m;
    sexp (Message.sexp_of_t m); (* TODO use debug mode here and everywhere else *)
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










