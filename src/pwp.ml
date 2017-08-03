open Core
open Async
open Log.Global

module M = Message
module P = Peer
module G = Global

open Meta_state

type t = {
  mutable has_meta : bool; (* convenience field *)
  mutable meta : Meta_state.t Option.t;
  peers : (Peer_id.t, P.t) Hashtbl.t;
  mutable num_requested : int;
}

let create ?meta () = { 
  peers = Hashtbl.Poly.create (); 
  meta; 
  has_meta = Option.is_some meta; 
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

let request t (i, p) = 
  t.num_requested <- t.num_requested + 1;
  let meta = Option.value_exn t.meta in
  P.request_piece p meta i
  >>| fun () ->
  t.num_requested <- t.num_requested - 1;
  send_have_messages t i 

(* Request as many pieces as we can. 

   If we reach this place, we have meta available. 
   There should be no more than G.max_pending_request. *)
let request_pieces t : unit =
  let meta = Option.value_exn t.meta in
  let n = G.max_pending_request - t.num_requested in
  if n > 0 then 
    let r = Strategy.next_requests meta.file t.peers n in
    Deferred.List.iter ~how:`Parallel ~f:(request t) r 
    |> don't_wait_for

let rec process_events t p meta : unit Deferred.t = 

  let%bind e = P.read_event p in
  info !"Pwp: got %{P.event_to_string} from %{P}" e p;
  match e with 
  | Bye -> 
    info !"Pwp: %{P} has left" p;
    P.id p |> Hashtbl.remove t.peers |> return 
  | _ -> 
    request_pieces t;
    process_events t p meta

let init t p meta = 
  
(*   if (File.num_downloaded_pieces meta.file) > 0 then (
    info !"Start: %{Peer} sending bitfield" p;
    File.bitfield meta.file |> P.send_bitfield p 
  );
  P.set_am_interested p true;
  P.set_am_choking p false;
 *)
  P.start p meta;

  process_events t p meta |> Deferred.ok

let add_peer t p =

  info !"Pwp: %{Peer} added" p;

  if not t.has_meta then
    failwith "Aie";

  let meta = Option.value_exn t.meta in
  P.set_num_pieces p meta.torrent.Torrent.num_pieces;

  let peer_id = P.id p in
  match Hashtbl.add t.peers ~key:peer_id ~data:p with 
  | `Ok  -> init t p meta  

  | `Duplicate -> 
    (* we ignore all peers already connected, and ourselves. It may be the case
       that the calling layers try to add twice the same peer. For instance,
       the tracker can return our own address and we may try to connect to 
       ourselves  *)
    Error (Error.of_string "ignore peers (already added or ourselves)") 
    |> return


