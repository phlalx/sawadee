open Core
open Async
open Log.Global

module M = Message
module P = Peer
module G = Global
module Nf = Network_file

type t = {
  info_hash : Bt_hash.t;
  mutable nf : Nf.t option;
  mutable peers : P.t Set.Poly.t;
  event_wr : (Peer.event * Peer.t) Pipe.Writer.t; 
  event_rd : (Peer.event * Peer.t) Pipe.Reader.t;
  mutable tinfo : Torrent.info option; (* TODO redondency with NF *)
  uris : Uri.t list option;
  peer_rd : (Peer_comm.t * Peer_comm.handshake_info) Pipe.Reader.t;
  peer_producer : Peer_producer.t;
}

let for_all_peers t ~f = Set.iter t.peers ~f

let send_have_messages t i =
  let notify_if_doesn't_have i p =
    if not (P.has_piece p i) then (
      debug !"Pwp: notify peer %{P} about piece %d" p i;
      P.send_have p i
    ) in
  for_all_peers t ~f:(notify_if_doesn't_have i)

let setup_download t nf p =
  P.set_nf p nf;
  if Nf.has_any_piece nf then (
    info !"Pwp: sending bitfield %{Peer}"  p;
    Nf.downloaded nf |> P.send_bitfield p;
  )

let event_loop_no_tinfo t () = 

  let process_event e p = 
    let open Peer in
    debug !"Pwp: event (no tinfo) %{P.event_to_string} from %{P}" e p;
    match e with 
    | Tinfo tinfo -> 
      info !"Pwp: got tinfo from %{P}" p;
      `Finished (Some tinfo)
    | Support_meta -> 
      P.request_meta p; 
      `Repeat ()
    | _ -> failwith (Peer.event_to_string e)
  in 

  match%map Pipe.read t.event_rd with
  | `Eof -> `Finished None
  | `Ok (e, p) -> process_event e p

let event_loop_tinfo t nf () = 

  let process_event t nf e p = 
    let open Peer in
    debug !"Pwp: process event %{P.event_to_string} from %{P}" e p;
    match e with 
    | Piece i -> 
      info !"Pwp: %{P} downloaded piece %d" p i;
      send_have_messages t i

    | Choke  -> ()

    | Not_interested -> ()

    | Interested -> 
      info !"Pwp: unchoking %{Peer}" p;
      P.set_am_choking p false

    | Unchoke ->
      info !"Pwp: %{P} unchoked us" p

    | Bitfield ->
      (* TODO validate bitfield here *)
      if (P.is_interesting p)  then (
        assert (not (P.am_interested p)); (* can't be interested before his bitfield *)
        P.set_am_interested p true
      )

    | Have i ->
      if not (Nf.is_downloaded nf i) then (
        if not (P.am_interested p) then (
          P.set_am_interested p true;
        )
      )

    | Support_meta -> assert false
    | Tinfo _  -> assert false

  in
  match%map Pipe.read t.event_rd with
  | `Eof -> 
    `Finished ()
  | `Ok (e, p) -> 
    process_event t nf e p;
    `Repeat ()

let status t = 
  let f nf = {
    Status.tinfo = Nf.tinfo nf;
    Status.downloaded = Nf.downloaded nf;
  } in {
    Status.peers = Set.to_list t.peers |> List.map ~f:Peer.status;
    Status.torrent = Option.map t.nf ~f;
  } 

let start_without_info t : Torrent.info Deferred.Option.t = 
  info !"Pwp: start peer events loop - without tinfo"; 
  Deferred.repeat_until_finished () (event_loop_no_tinfo t)

let start_with_tinfo t (tinfo : Torrent.info) : unit Deferred.t =

  let n = Bt_hash.to_hex t.info_hash |> G.torrent_name |> G.with_torrent_path in
  info "Pwp: saving meta-info to file %s" n; 
  Torrent.info_to_string tinfo |> Out_channel.write_all n;

  let%bind nf = Nf.create t.info_hash tinfo in

  t.nf <- Some nf;
  setup_download t nf |> for_all_peers t;
(* 
  info "Pwp: start pieces requesting loop"; 
  Deferred.forever () (request_pieces t nf); *)

  info "Pwp: start peer events loop - with tinfo"; 
  Deferred.repeat_until_finished () (event_loop_tinfo t nf)

let add_peer_comm t (pc : Peer_comm.t) (hi : Peer_comm.handshake_info) :
  unit Deferred.Or_error.t
  =
  (let p = Peer.create hi.peer_id pc ~extension:hi.extension ~dht:hi.dht in 
   t.peers <- Set.add t.peers p;
   info !"Pwp: %{Peer} added (%d in) has_nf %b" p (Set.length t.peers) (Option.is_some t.nf);
   Option.iter t.nf (fun nf -> setup_download t nf p);
   P.start p |> don't_wait_for;
   (* finishes when pipe is closed *)
   Pipe.transfer (Peer.event_reader p) t.event_wr ~f:(fun e -> (e, p)) 
   >>| fun () -> 
   t.peers <- Set.remove t.peers p;
   info !"Pwp: %{P} has left (%d left)" p (Set.length t.peers))
  |> Deferred.ok

let rec process_peers t () = 
  match%bind Pipe.read t.peer_rd with
  | `Eof -> `Finished () |> return
  | `Ok (p, hi) -> 
    info !"Pwp: %{Peer_comm} read from pipe" p; 
    add_peer_comm t p hi |> Deferred.ignore |> don't_wait_for; 
    Clock.after (sec 5.) 
    >>| fun () ->
    `Repeat () 

(* 
let rec process_peers t () = 
  match%bind Pipe.read t.peer_rd with
  | `Eof -> `Finished () |> return
  | `Ok (p, hi) -> 
    info !"Pwp: %{Peer_comm} read from pipe" p; 
    add_peer_comm t p hi 
    >>| fun _ -> 
    info !"Pwp: %{Peer_comm} terminate, can add new peer" p; 
    `Repeat ()  *)

let stop t = assert false

let start t =
  let tinfo : Torrent.info Deferred.Option.t =
    match t.tinfo with 
    | None -> start_without_info t
    | Some tinfo -> Some tinfo |> return 
  in
  Deferred.Option.Monad_infix.(tinfo >>| start_with_tinfo t)
  |> Deferred.ignore |> don't_wait_for;

  Peer_producer.start t.peer_producer; 
  Deferred.repeat_until_finished () (process_peers t)
  |> don't_wait_for

let close t = 
  info !"Pwp: closing %{Bt_hash.to_hex}" t.info_hash;
  Set.to_list t.peers |> Deferred.List.iter ~f:Peer.close
  >>= fun () ->
  Pipe.close t.event_wr; 
  match t.nf with 
  | None -> Deferred.unit  
  | Some nf -> Nf.close nf  

let create uris tinfo info_hash = 
  info !"Pwp: create %{Bt_hash.to_hex}" info_hash;
  let event_rd, event_wr = Pipe.create () in 
  let peer_rd, peer_wr = Pipe.create () in
  let peer_producer = Peer_producer.create peer_wr info_hash uris in
  { 
    info_hash;
    peers = Set.Poly.empty; (* TODO use a set based on peer-id *)
    nf = None;
    event_rd;
    event_wr;
    uris;
    peer_producer;
    peer_rd;
    tinfo;
  }

