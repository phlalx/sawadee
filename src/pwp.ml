open Core
open Async
open Log.Global

module M = Message
module P = Peer
module G = Global

open Network_file

type t = {
  mutable has_nf : bool; (* convenience field *)
  mutable nf : Network_file.t Option.t;
  peers : (Peer_id.t, P.t) Hashtbl.t;
  mutable num_requested : int;
}

let create ?nf () = { 
  peers = Hashtbl.Poly.create (); 
  nf; 
  has_nf = Option.is_some nf; 
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

let rec request t (i, p) : unit Deferred.t = 
  t.num_requested <- t.num_requested + 1;
  let nf = Option.value_exn t.nf in
  P.request_piece p nf i
  >>| fun () ->
  t.num_requested <- t.num_requested - 1;
  send_have_messages t i; 
  request_pieces t

(* Request as many pieces as we can. 

   If we reach this place, we have nf available. 
   There should be no more than G.max_pending_request. *)
and request_pieces t : unit =
  let nf = Option.value_exn t.nf in
  let n = G.max_pending_request - t.num_requested in
  if n > 0 then 
    let r = Strategy.next_requests nf t.peers n in
    Deferred.List.iter ~how:`Parallel ~f:(request t) r 
    |> don't_wait_for

let rec process_events t p nf : unit Deferred.t = 

  let%bind e = P.read_event p in
  info !"Pwp: got %{P.event_to_string} from %{P}" e p;
  match e with 
  | Bye -> 
    info !"Pwp: %{P} has left" p;
    P.id p |> Hashtbl.remove t.peers |> return 
  | _ -> 

(*     let one_percent = max (nf.torrent.Torrent.num_pieces / 100) 1 in
    if (File.num_downloaded_pieces nf.file % one_percent) = 0 then
      info "Pwp: downloaded %d%%" (File.percent nf.file);
 *) 
    request_pieces t;
    process_events t p nf

let init t p nf = 

  if (Network_file.num_downloaded_pieces nf) > 0 then (
    info !"Pwp: %{Peer} sending bitfield" p;
    Network_file.downloaded nf |> P.send_bitfield p 
  );
  P.set_am_interested p true;
  P.set_am_choking p false;

  P.start p nf;

  process_events t p nf |> Deferred.ok

let add_peer t p =

  info !"Pwp: %{Peer} added" p;

  if not t.has_nf then
    failwith "Aie";

  let nf = Option.value_exn t.nf in

  let peer_id = P.id p in
  match Hashtbl.add t.peers ~key:peer_id ~data:p with 
  | `Ok  -> init t p nf  

  | `Duplicate -> 
    (* we ignore all peers already connected, and ourselves. It may be the case
       that the calling layers try to add twice the same peer. For instance,
       the tracker can return our own address and we may try to connect to 
       ourselves  *)
    Error (Error.of_string "ignore peers (already added or ourselves)") 
    |> return


