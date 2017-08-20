open Core
open Async
open Log.Global

module G = Global
module Nf = Network_file
module Pp = Peer_producer

type t = {
  info_hash : Bt_hash.t;
  mutable nf : Nf.t option;
  mutable peers : Peer.t Set.Poly.t;
  event_wr : (Pevent.t * Peer.t) Pipe.Writer.t; 
  event_rd : (Pevent.t * Peer.t) Pipe.Reader.t;
  mutable tinfo : Torrent.info option; (* TODO redondency with NF *)
  uris : Uri.t list option;
  peer_rd : (Peer_comm.t * Peer_comm.handshake_info) Pipe.Reader.t;
  peer_producer : Pp.t;
}

let to_string t = Bt_hash.to_hex t.info_hash

let for_all_peers t ~f = Set.iter t.peers ~f

let remove_peer t p =
  t.peers <- Set.remove t.peers p;
  info !"Pwp: %{Peer} has left (%d left)" p (Set.length t.peers)

let event_loop_no_tinfo t () = 

  let process_event e p = 
    let open Peer in
    debug !"Pwp: event (no tinfo) %{Pevent} from %{Peer}" e p;
    match e with 
    | Tinfo tinfo -> 
      `Finished (Some tinfo)
    | Support_meta -> 
      Peer.request_meta p; 
      `Repeat ()
    | Bye ->
      remove_peer t p;
      `Repeat ()
    | Piece _ -> assert false
  in 

  match%map Pipe.read t.event_rd with
  | `Eof -> `Finished None
  | `Ok (e, p) -> process_event e p

let event_loop_tinfo t nf () = 

  let process_event t nf e p = 
    let open Peer in
    debug !"Pwp: process event %{Pevent} from %{Peer}" e p;
    match e with 
    | Piece i -> 
      for_all_peers t ~f:(fun p -> Peer.send_have p i)
    | Bye ->
      remove_peer t p
    | Support_meta | Tinfo _ -> assert false

  in
  match%map Pipe.read t.event_rd with
  | `Eof -> 
    `Finished ()
  | `Ok (e, p) -> 
    process_event t nf e p;
    `Repeat ()
(* TODO try to use Pipe.iter *)

let start_without_info t : Torrent.info Deferred.Option.t = 
  info !"Pwp: %{} start event loop - without tinfo" t; 
  Deferred.repeat_until_finished () (event_loop_no_tinfo t)

let start_with_tinfo t (tinfo : Torrent.info) : unit Deferred.t =

  let n = Bt_hash.to_hex t.info_hash |> G.torrent_name |> G.with_torrent_path in
  info "Pwp: saving meta-info to file %s" n; 
  Torrent.info_to_string tinfo |> Out_channel.write_all n;

  let%bind nf = Nf.create t.info_hash tinfo in
  let f dht = 
    Dht.announce dht t.info_hash (G.port_exn ())
  in
  Option.iter (G.dht ()) ~f;
  t.nf <- Some nf;
  for_all_peers t (fun p -> Peer.set_nf p nf);

  info !"Pwp: %{} start event loop - with tinfo" t; 
  Deferred.repeat_until_finished () (event_loop_tinfo t nf)

let add_peer_comm t (pc : Peer_comm.t) (hi : Peer_comm.handshake_info) =
  let p = Peer.create hi.peer_id pc t.nf t.event_wr ~extension:hi.extension 
      ~dht:hi.dht in t.peers <- Set.add t.peers p;
  info !"Pwp: %{Peer} added (%d in) has_nf %b" p (Set.length t.peers) (Option.is_some t.nf);
  Peer.start p

let rec process_peers t () = 
  match%bind Pipe.read t.peer_rd with
  | `Eof -> `Finished () |> return
  | `Ok (p, hi) -> 
    add_peer_comm t p hi |> don't_wait_for;
    (* Clock.after (sec 1.)  *)    (* TODO not necessary, just for debug *)
    `Repeat () |> return 

let stop t = assert false

let start t =
  info !"Pwp: start %{}" t;
  let tinfo : Torrent.info Deferred.Option.t =
    match t.tinfo with 
    | None -> start_without_info t
    | Some tinfo -> Some tinfo |> return 
  in
  Deferred.Option.Monad_infix.(tinfo >>| start_with_tinfo t)
  |> Deferred.ignore |> don't_wait_for;

  Pp.start t.peer_producer; 
  Deferred.repeat_until_finished () (process_peers t)
  |> don't_wait_for

let close t = 
  info !"Pwp: closing %{}" t;
  Set.to_list t.peers |> Deferred.List.iter ~f:Peer.close
  >>= fun () ->
  Pipe.close t.event_wr; 
  match t.nf with 
  | None -> Deferred.unit  
  | Some nf -> Nf.close nf  

let create uris tinfo info_hash = 
  info !"Pwp: create with info_hash %{Bt_hash.to_hex}" info_hash;
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

let status t = 
  let f nf = {
    Status.tinfo = Nf.tinfo nf;
    Status.downloaded = Nf.downloaded nf;
  } in {
    Status.peers = Set.to_list t.peers |> List.map ~f:Peer.status;
    Status.torrent = Option.map t.nf ~f;
  } 
