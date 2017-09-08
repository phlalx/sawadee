open Core
open Async
open Log.Global

module M = Message
module P = Peer
module G = Global
module Nf = Network_file

type t = {
  info_hash : Bt_hash.t;
  has_nf : unit Ivar.t;
  mutable nf : Nf.t Option.t;
  peers : (Peer_id.t, P.t) Hashtbl.t;
  requested : (int, unit Ivar.t) Hashtbl.t;
}

let set_nf t tinfo = 
  let%map nf = Nf.create t.info_hash tinfo in
  t.nf <- Some nf;
  Ivar.fill t.has_nf ()

let create info_hash = { 
    info_hash;
    peers = Hashtbl.Poly.create (); 
    nf = None;
    has_nf = Ivar.create ();
    requested = Hashtbl.Poly.create ();
  }

let for_all_peers t ~f = Hashtbl.iter t.peers ~f

let send_have_messages t i =
  let notify_if_doesn't_have i p =
    if not (P.has_piece p i) then (
      debug !"Pwp: notify peer %{P} about piece %d" p i;
      P.advertise_piece p i
    ) in
  for_all_peers t ~f:(notify_if_doesn't_have i)

let rec request t nf (i, p): unit Deferred.t = 
  info !"Pwp: %{P} requesting piece %d" p i;

  let on_ivar = function 
    | `Timeout -> 
      info !"Pwp: %{Peer} failed to provide piece %d" p i;
      Peer.set_idle p true;
    | `Result () -> 
      info !"Pwp: %{Peer} gave us piece %d" p i;
      Nf.set_downloaded nf i;
      Nf.write_piece nf i;
      send_have_messages t i;
      request_pieces t nf
  in 

  let ivar = Ivar.create () in
  Hashtbl.add_exn t.requested ~key:i ~data:ivar;
  P.request_piece p i;
  Ivar.read ivar |> Clock.with_timeout G.idle  
  >>| 
  on_ivar 
  >>| fun () ->
  Hashtbl.remove t.requested i;

(* Request as many pieces as we can. *)
and request_pieces t nf : unit =
  let num_requested = Hashtbl.length t.requested in
  let n = G.max_pending_request - num_requested in
  if n > 0 then 
    let peers = Hashtbl.to_alist t.peers |> List.map ~f:snd in
    let l = List.range 0 (Nf.num_pieces nf) |> 
            List.filter ~f:(fun i -> not (Nf.has_piece nf i)) |>
            List.filter ~f:(fun i -> not (Hashtbl.mem t.requested i))
          in
    Strategy.next_requests l peers n  
    |> Deferred.List.iter ~how:`Parallel ~f:(request t nf) 
    |> don't_wait_for

(* read events from the peers to see if we should request try to request
   pieces *)
let rec process_events t p : unit Deferred.t = 

  let%bind e = P.read_event p in
  info !"Pwp: event %{P.event_to_string} from %{P}" e p;
  match e with 
  | Bye -> 
    info !"Pwp: %{P} has left" p;
    P.id p |> Hashtbl.remove t.peers |> return 
  | Piece i -> (
    match Hashtbl.find t.requested i with 
    | None -> return ()
    | Some i -> Ivar.fill i (); return ())
  | _ -> 
    Option.value_map ~default:() t.nf ~f:(request_pieces t);
    process_events t p 

let init t p  = 

  (* we need to start the event loop of p to get the possible extension 
     message *)
  P.start p;

  (* at that stage we only care about event Bye *)
  process_events t p |> don't_wait_for;

  info !"Pwp: %{Peer} waiting for nf" p;
  (* Ivar.read t.has_nf  *)
  (* TODO *)
  never ()
  >>= fun () ->
  info !"Pwp: %{Peer} yeah! can proceed" p;

  (* TODO add some code that can request the missing nf *)
  let nf = Option.value_exn t.nf in
  Peer.set_nf p nf;

  if (Nf.num_downloaded_pieces nf) > 0 then (
    info !"Pwp: %{Peer} sending bitfield" p;
    Nf.downloaded nf |> P.send_bitfield p 
  );

  P.set_am_interested p true;
  P.set_am_choking p false;
  Ok () |> return

let add_peer t p =
  info !"Pwp: %{Peer} added" p;
  let peer_id = P.id p in

  match Hashtbl.add t.peers ~key:peer_id ~data:p with 
  | `Ok  -> init t p   

  | `Duplicate -> 
    (* we ignore all peers already connected, and ourselves. It may be the case
       that the calling layers try to add twice the same peer. For instance,
       the tracker can return our own address and we may try to connect to 
       ourselves  *)
    Error (Error.of_string "ignore peers (already added or ourselves)") 
    |> return

let close t = 
  match t.nf with 
  | None -> Deferred.unit  
  | Some nf -> Nf.close nf  

let status t = Status.{
  num_peers = Hashtbl.length t.peers  
} 





