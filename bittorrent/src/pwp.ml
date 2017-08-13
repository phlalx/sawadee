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
  mutable requested : int Set.Poly.t;
  wr : (Peer.event * Peer.t) Pipe.Writer.t; 
  rd : (Peer.event * Peer.t) Pipe.Reader.t;
  possible_request : unit Condition.t
}

let for_all_peers t ~f = Set.iter t.peers ~f

let send_have_messages t i =
  let notify_if_doesn't_have i p =
    if not (P.has_piece p i) then (
      debug !"Pwp: notify peer %{P} about piece %d" p i;
      P.send_have p i
    ) in
  for_all_peers t ~f:(notify_if_doesn't_have i)

let request t nf (i, p): unit = 
  info !"Pwp: %{P} requesting piece %d" p i;
  assert (not (Set.mem t.requested i));
  t.requested <- Set.add t.requested i;
  P.request_piece p i 

(* Request as many pieces as we can. *)
let request_pieces t nf () =

  Condition.wait t.possible_request
  >>| fun () ->
  let n = 20 in
  info "Pwp: try to request up to %d pieces." n;
  let peers = Set.to_list t.peers in
  let l = List.range 0 (Nf.num_pieces nf) |> 
          List.filter ~f:(fun i -> not (Nf.has_piece nf i)) |>
          List.filter ~f:(fun i -> not (Set.mem t.requested i))
  in
  let r = Strategy.next_requests l peers n in
  List.iter ~f:(request t nf) r

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

  match%map Pipe.read t.rd with
  | `Eof -> `Finished None
  | `Ok (e, p) -> process_event e p

let event_loop_tinfo t nf () = 

  let process_event t nf e p = 
    let open Peer in
    debug !"Pwp: process event %{P.event_to_string} from %{P}" e p;
    match e with 
    | Piece i -> 
      info !"Pwp: %{P} downloaded piece %d" p i;
      Nf.set_downloaded nf i;
      Nf.write_piece nf i;
      send_have_messages t i;
      t.requested <- Set.remove t.requested i;

    | Fail i ->
      t.requested <- Set.remove t.requested i;

    | Choke  -> ()

    | More  -> Condition.signal t.possible_request ()

    | Not_interested -> ()

    | Interested -> 
      info !"Pwp: unchoking %{Peer}" p;
      P.set_am_choking p false

    | Unchoke ->
      info !"Pwp: %{P} unchoked us" p;
      Condition.signal t.possible_request ()

    | Bitfield ->
      (* TODO validate bitfield here *)
      if (P.is_interesting p)  then (
        assert (not (P.am_interested p)); (* can't be interested before his bitfield *)
        P.set_am_interested p true
      )

    | Have i ->
      if not (Nf.has_piece nf i) then (
        if not (P.am_interested p) then (
          P.set_am_interested p true;
        );
        if not (P.peer_choking p) then (
          Condition.signal t.possible_request ()
        )
      )

    | Support_meta -> assert false
    | Tinfo _  -> assert false

  in
  match%map Pipe.read t.rd with
  | `Eof -> 
    `Finished ()
  | `Ok (e, p) -> 
    process_event t nf e p;
    `Repeat ()

let add_peer t p =
  t.peers <- Set.add t.peers p;
  info !"Pwp: %{Peer} added (%d in) has_nf %b" p (Set.length t.peers) (Option.is_some t.nf);
  Option.iter t.nf (fun nf -> setup_download t nf p);
  P.start p |> don't_wait_for;
  Pipe.transfer (Peer.event_reader p) t.wr ~f:(fun e -> (e, p)) 
  >>| fun () -> 
  t.peers <- Set.remove t.peers p;
  info !"Pwp: %{P} has left (%d left)" p (Set.length t.peers)

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

  info "Pwp: start pieces requesting loop"; 
  Deferred.forever () (request_pieces t nf);

  info "Pwp: start peer events loop - with tinfo"; 
  Deferred.repeat_until_finished () (event_loop_tinfo t nf)

let start t tinfo_opt =
  let tinfo : Torrent.info Deferred.Option.t =
    match tinfo_opt with 
    | None -> start_without_info t
    | Some tinfo -> Some tinfo |> return 
  in
  Deferred.Option.Monad_infix.(tinfo >>| start_with_tinfo t)
  |> Deferred.ignore

let close t = 
  info !"Pwp: closing %{Bt_hash.to_hex}" t.info_hash;
  Set.to_list t.peers |> Deferred.List.iter ~f:Peer.close
  >>= fun () ->
  Pipe.close t.wr; 
  match t.nf with 
  | None -> Deferred.unit  
  | Some nf -> Nf.close nf  

let create info_hash = 
  info !"Pwp: create %{Bt_hash.to_hex}" info_hash;
  let rd, wr = Pipe.create () in 
  let possible_request = Condition.create () in { 
    info_hash;
    peers = Set.Poly.empty; (* TODO use a set based on peer-id *)
    nf = None;
    requested = Set.Poly.empty;
    rd;
    wr;
    possible_request;
  }

