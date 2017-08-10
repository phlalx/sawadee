open Core
open Async
open Log.Global

module M = Message
module P = Peer
module G = Global
module Nf = Network_file

type t = {
  info_hash : Bt_hash.t;
  mutable nf : Nf.t Option.t;
  mutable peers : P.t Set.Poly.t;
  requested : (int, unit Ivar.t) Hashtbl.t;
  wr : (Peer.event * Peer.t) Pipe.Writer.t; 
  rd : (Peer.event * Peer.t) Pipe.Reader.t;
  sigpiece_wr : unit Pipe.Writer.t; 
  sigpiece_rd : unit Pipe.Reader.t; 
}

let for_all_peers t ~f = Set.iter t.peers ~f

let send_have_messages t i =
  let notify_if_doesn't_have i p =
    if not (P.has_piece p i) then (
      debug !"Pwp: notify peer %{P} about piece %d" p i;
      P.advertise_piece p i
    ) in
  for_all_peers t ~f:(notify_if_doesn't_have i)

let signal_piece t =
  Pipe.write_without_pushback t.sigpiece_wr ()

let request t nf (i, p): unit Deferred.t = 
  info !"Pwp: %{P} requesting piece %d" p i;

  let on_ivar = function 
    | `Timeout -> 
      info !"Pwp: %{Peer} failed to provide piece %d" p i;
      Peer.set_idle p true 
    | `Result () -> () 
  in 

  let ivar = Ivar.create () in
  Hashtbl.add_exn t.requested ~key:i ~data:ivar;
  P.request_piece p i;
  Ivar.read ivar |> Clock.with_timeout G.idle  
  >>| 
  on_ivar 
  >>| fun () ->
  Hashtbl.remove t.requested i;
  signal_piece t

(* Request as many pieces as we can. *)
let request_pieces t nf =

  let try_request () =
    let num_requested = Hashtbl.length t.requested in
    let n = G.max_pending_request - num_requested in
    if n > 0 then ( 
      info "Pwp: try to request up to %d pieces." n;
      let peers = Set.to_list t.peers in
      let l = List.range 0 (Nf.num_pieces nf) |> 
              List.filter ~f:(fun i -> not (Nf.has_piece nf i)) |>
              List.filter ~f:(fun i -> not (Hashtbl.mem t.requested i))
      in
      Strategy.next_requests l peers n  
      |> Deferred.List.iter ~how:`Parallel ~f:(request t nf))
      |> don't_wait_for
  in

  match%map Pipe.read' t.sigpiece_rd with
  | `Eof -> 
    `Finished () 
  | `Ok _q -> 
    (* info "Pwp: trying to request (%d signals)" (Queue.length q); *)
    try_request ();
    `Repeat () 

let setup_download t nf p =
  P.set_nf p nf;
  if (Nf.num_downloaded_pieces nf) > 0 then (
    info !"Pwp: sending bitfield %{Peer}"  p;
    Nf.downloaded nf |> P.send_bitfield p 
  );
  info !"Pwp: interested in %{Peer}" p;
  P.set_am_interested p true;
  info !"Pwp: unchoking %{Peer}" p;
  P.set_am_choking p false

let rec process t f =
  match%map Pipe.read t.rd with
  | `Eof -> 
    `Finished None
  | `Ok (e, p) -> f t e p

let without_nf t e p = 
  let open Peer in
  debug !"Pwp: event (no tinfo) %{P.event_to_string} from %{P}" e p;
  match e with 
  | Tinfo tinfo -> 
    info !"Pwp: got tinfo from %{P}" p;
    `Finished (Some tinfo)
  | Support_meta -> 
    P.request_meta p; 
    `Repeat ()
  | _ -> 
    `Repeat ()

let with_nf nf t e p = 
  let open Peer in
  debug !"Pwp: event (tinfo) %{P.event_to_string} from %{P}" e p;
  match e with 
  | Bye -> 
    `Repeat ()
  | Join -> 
    setup_download t nf p; `Repeat ()
  | Piece i -> 
    begin
      info !"Pwp: %{P} downloaded piece %d" p i;
      match Hashtbl.find t.requested i with 
      | None -> 
        (* TODO keep the piece anyway *)
        info !"Pwp: %{P} downloaded piece %d - for nothing" p i
      | Some iv -> 
        info !"Pwp: %{P} downloaded piece %d" p i;
        Ivar.fill iv (); 
        Nf.set_downloaded nf i;
        Nf.write_piece nf i;
        send_have_messages t i
    end; 
    `Repeat ()
  | Choke -> 
    `Repeat ()
  | Support_meta | Tinfo _ | Interested | Not_interested -> 
    `Repeat ()
  | Unchoke ->
    info !"Pwp: %{P} unchoked us" p;

    signal_piece t;
    `Repeat ()
  | Bitfield bf ->
    let l = Bitfield.to_list bf (Nf.num_pieces nf) in
    signal_piece t;
    `Repeat ()
  | Have i ->
    signal_piece t;
    `Repeat ()

let add_peer t p =
  t.peers <- Set.add t.peers p;
  info !"Pwp: %{Peer} added (%d in)" p (Set.length t.peers);
  P.start p |> don't_wait_for;
  Pipe.transfer (Peer.event_reader p) t.wr ~f:(fun e -> (e, p)) 
  >>| fun () -> 
  t.peers <- Set.remove t.peers p;
  info !"Pwp: %{P} has left (%d left)" p (Set.length t.peers)

let status t = Status.{
    num_peers = Set.length t.peers; 
    num_downloaded_pieces = Option.value_exn t.nf |> Nf.num_downloaded_pieces
  } 

let start_opt t tinfo = 
  let open Deferred.Option.Monad_infix in
  (match tinfo with 
   | None ->
     info !"Pwp: start message handler 'without info' loop"; 
     Deferred.repeat_until_finished () (fun () -> process t without_nf)
   | Some tinfo -> Some tinfo |> return )
  >>= fun tinfo ->

  let n = t.info_hash |> Bt_hash.to_hex |> G.torrent_name 
          |> G.with_torrent_path in
  info "Pwp: saving meta-info to file %s" n; 
  Torrent.info_to_string tinfo |> Out_channel.write_all n;

  let%bind nf = Nf.create t.info_hash tinfo in
  t.nf <- Some nf;

  (* TODO this is done twice for peers that haven't joined yet *)
  setup_download t nf |> for_all_peers t;

  let to_download = 
    List.range 0 (Nf.num_pieces nf)   
    |> List.filter ~f:(fun i -> not (Nf.has_piece nf i)) 
  in

  info "Pwp: start piece_signal loop"; 
  Deferred.repeat_until_finished () (fun () -> request_pieces t nf)
  |> don't_wait_for;

  info "Pwp: start message handler 'with info' loop"; 
  Deferred.repeat_until_finished () (fun () -> process t (with_nf nf))

let start t tinfo = 
  start_opt t tinfo |> Deferred.ignore

let close t = 
  info !"Pwp: closing %{Bt_hash.to_hex}" t.info_hash;
  Set.to_list t.peers |> Deferred.List.iter ~f:Peer.close
  >>= fun () ->
  Pipe.close t.sigpiece_wr; (* TODO can we discard what's in the pipe *)
  Pipe.close t.wr; 
  match t.nf with 
  | None -> Deferred.unit  
  | Some nf -> Nf.close nf  

let create info_hash = 
  info !"Pwp: create %{Bt_hash.to_hex}" info_hash;
  let rd, wr = Pipe.create () in 
  let sigpiece_rd, sigpiece_wr = Pipe.create () in 

  { 
    info_hash;
    peers = Set.Poly.empty; (* TODO use a set based on peer-id *)
    nf = None;
    requested = Hashtbl.Poly.create ();
    rd;
    wr;
    sigpiece_rd;
    sigpiece_wr;
  }


