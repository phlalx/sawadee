open Core
open Async
open Blog

module G = Global

type t = {
  info_hash : Bt_hash.t;
  mutable sm : Shared_meta.t option;
  mutable peers : Peer.t Set.Poly.t;
  event_wr : (Pevent.t * Peer.t) Pipe.Writer.t; 
  event_rd : (Pevent.t * Peer.t) Pipe.Reader.t;
  peer_rd : (Peer_comm.t * Peer_comm.handshake_info) Pipe.Reader.t;
  peer_producer : Peer_producer.t;
  peer_producer_gate : unit Mvar.Read_write.t
}

let to_string t = Bt_hash.to_hex t.info_hash

let for_all_peers t ~f = Set.iter t.peers ~f

let remove_peer t p =
  t.peers <- Set.remove t.peers p;
  info !"Swarm: %{Peer} has left (%d left)" p (Set.length t.peers)

let event_loop_no_sm t () = 

  let process_event e p = 
    let open Peer in
    debug !"Swarm: event (no tinfo) %{Pevent} from %{Peer}" e p;
    match e with 
    | Tinfo tinfo -> 
      `Finished (Some tinfo)
    | Support_meta -> 
      Peer.request_meta_info p; 
      `Repeat ()
    | Bye ->
      remove_peer t p;
      `Repeat ()
    | Piece _ -> assert false
  in 

  match%map Pipe.read t.event_rd with
  | `Eof -> `Finished None
  | `Ok (e, p) -> process_event e p

let event_loop_sm t sm () = 

  let process_event t sm e p = 
    let open Peer in
    debug !"Swarm: process event %{Pevent} from %{Peer}" e p;
    match e with 
    | Piece i -> 
      for_all_peers t ~f:(fun p -> Peer.notify p i);
      if (Shared_meta.seeder sm) then (
        debug !"Swarm: last piece downloaded. Block peer_producer.";
        Mvar.take t.peer_producer_gate |> ignore
      )
    | Bye ->
      remove_peer t p
    | Support_meta | Tinfo _ -> ()
  in
  match%map Pipe.read t.event_rd with
  | `Eof -> 
    `Finished ()
  | `Ok (e, p) -> 
    process_event t sm e p;
    `Repeat ()
(* TODO try to use Pipe.iter *)

let start_without_sm t : Shared_meta.t Deferred.Option.t = 
  info !"Swarm: %{} start event loop - without sm" t; 
  match%bind Deferred.repeat_until_finished () (event_loop_no_sm t) with
  | None -> return None
  | Some tinfo -> 
    info !"Swarm: %{} got meta-info" t; 
    let n = Bt_hash.to_hex t.info_hash |> G.torrent_name |> G.with_torrent_path
    in
    info "Swarm: saving meta-info to file %s" n; 
    Torrent.info_to_string tinfo |> Out_channel.write_all n;

    let%map sm = Shared_meta.create ~seeder:false t.info_hash tinfo in Some sm

let start_with_sm t sm : unit =

  let f dht = 
    Dht.announce dht t.info_hash (G.port_exn ())
  in
  Option.iter (G.dht ()) ~f;
  t.sm <- Some sm;
  for_all_peers t (fun p -> Peer.set_shared_meta p sm);

  info !"Swarm: %{} start event loop - with sm" t; 
  Deferred.repeat_until_finished () (event_loop_sm t sm)
  |> don't_wait_for

let add_peer_comm t (pc : Peer_comm.t) (hi : Peer_comm.handshake_info) =

  (* TODO use a proper set structure and move this test before? *)

  let ids = Set.to_list t.peers |> List.map ~f:Peer.id in
  let id = hi.peer_id in

  if List.mem ~equal:(=) ids id then (
    info !"Swarm: ignore %{Peer_id.to_string_hum}, already added" id;
    Peer_comm.close pc 
  ) else (
    let p = Peer.create t.info_hash hi.peer_id pc t.sm t.event_wr 
        ~extension:hi.extension ~dht:hi.dht in 
    t.peers <- Set.add t.peers p;
    info !"Swarm: %{Peer} added (%d in)" p (Set.length t.peers);
    Peer.start p)

let rec consume_peers t () = 
  match%bind Pipe.read t.peer_rd with
  | `Eof -> `Finished () |> return
  | `Ok (p, hi) -> 
    add_peer_comm t p hi |> don't_wait_for;
    (* Clock.after (sec 1.)  *)    (* TODO not necessary, just for debug *)
    `Repeat () |> return 

let stop t = assert false

let get_sm t : Shared_meta.t Deferred.Option.t =
  match t.sm with
  | Some sm -> return (Some sm)
  | None -> start_without_sm t  

let start t : unit =
  (let open Deferred.Option.Let_syntax in
   info !"Swarm: start %{}" t;
   let%map sm = get_sm t in
   start_with_sm t sm)
  |> Deferred.ignore |> don't_wait_for;
  Peer_producer.start t.peer_producer; 
  (match t.sm with
   | Some sm when Shared_meta.seeder sm -> ()
   | _ -> 
     debug !"Swarm: not seeder so unblock peer_producer.";
     Mvar.set t.peer_producer_gate ()); (* fill mvar only if not seeder *)
  Deferred.repeat_until_finished () (consume_peers t) |> don't_wait_for

let close t = 
  info !"Swarm: closing %{}" t;
  Set.to_list t.peers |> Deferred.List.iter ~f:Peer.close
  >>= fun () ->
  Pipe.close t.event_wr; 
  match t.sm with 
  | None -> Deferred.unit  
  | Some sm -> Shared_meta.close sm  

let create uris sm info_hash = 
  info !"Swarm: create %{Bt_hash.to_string_hum} (sm = %b)" info_hash
    (Option.is_some sm);
  let event_rd, event_wr = Pipe.create () in 
  let peer_rd, peer_wr = Pipe.create () in
  let peer_producer_gate = Mvar.create () in
  let ppg_ro = Mvar.read_only peer_producer_gate in
  let peer_producer = Peer_producer.create ppg_ro peer_wr info_hash uris in
  { 
    info_hash;
    peers = Set.Poly.empty; (* TODO use a set based on peer-id *)
    sm;
    event_rd;
    event_wr;
    peer_producer;
    peer_rd;
    peer_producer_gate;
  }

let status t = 
  let f sm = {
    Status.tinfo = Shared_meta.tinfo sm;
    Status.downloaded = Shared_meta.downloaded sm;
  } in {
    Status.peers = Set.to_list t.peers |> List.map ~f:Peer.status;
    Status.torrent = Option.map t.sm ~f;
  } 
