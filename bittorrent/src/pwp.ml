open Core
open Async
open Log.Global

module M = Message
module P = Peer
module G = Global

open Network_file

type status = {
  num_peers : int
}

type t = {
  info_hash : Bt_hash.t;
  has_nf : unit Ivar.t;
  mutable nf : Network_file.t Option.t;
  peers : (Peer_id.t, P.t) Hashtbl.t;
  mutable num_requested : int;
}

let set_nf t tinfo = 
  let%map nf = Network_file.create t.info_hash tinfo in
  t.nf <- Some nf;
  Ivar.fill t.has_nf ()

let create info_hash =
  { 
    info_hash;
    peers = Hashtbl.Poly.create (); 
    nf = None;
    has_nf = Ivar.create ();
    num_requested = 0;
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
  t.num_requested <- t.num_requested + 1;
  P.request_piece p i
  >>| fun () ->
  t.num_requested <- t.num_requested - 1;
  send_have_messages t i;
  request_pieces t nf;
  assert false

(* Request as many pieces as we can. 

   If we reach this place, we have nf available. 
   There should be no more than G.max_pending_request. *)
and request_pieces t nf : unit =
  let n = G.max_pending_request - t.num_requested in
  if n > 0 then 
    Strategy.next_requests nf t.peers n  
    |> Deferred.List.iter ~how:`Parallel ~f:(request t nf) 
    |> don't_wait_for

(* read events from the peers to see if we should request try to request
   pieces *)
let rec process_events t p : unit Deferred.t = 

  let%bind e = P.read_event p in
  info !"Pwp: EVENT %{P.event_to_string} from %{P}" e p;
  match e with 
  | Bye -> 
    info !"Pwp: %{P} has left" p;
    P.id p |> Hashtbl.remove t.peers |> return 
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

  if (Network_file.num_downloaded_pieces nf) > 0 then (
    info !"Pwp: %{Peer} sending bitfield" p;
    Network_file.downloaded nf |> P.send_bitfield p 
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
  | Some nf -> Network_file.close nf  

let status t = Status.{
  num_peers = Hashtbl.length t.peers  
} 





