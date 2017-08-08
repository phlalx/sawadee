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
  peers : (Peer_id.t, P.t) Hashtbl.t;
  requested : (int, unit Ivar.t) Hashtbl.t;
  wr : (Peer.event * Peer.t) Pipe.Writer.t; 
  rd : (Peer.event * Peer.t) Pipe.Reader.t;
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
      send_have_messages t i
  in 

  let ivar = Ivar.create () in
  Hashtbl.add_exn t.requested ~key:i ~data:ivar;
  P.request_piece p i;
  Ivar.read ivar |> Clock.with_timeout G.idle  
  >>| 
  on_ivar 
  >>| fun () ->
  Hashtbl.remove t.requested i;
  request_pieces t nf (* we can try to request again as there's one less pending request *)

(* Request as many pieces as we can. *)
and request_pieces t nf : unit =
  let num_requested = Hashtbl.length t.requested in
  let n = G.max_pending_request - num_requested in
  if n > 0 then ( 
    info "Pwp: try to request up to %d pieces." n;
    let peers = Hashtbl.to_alist t.peers |> List.map ~f:snd in
    let l = List.range 0 (Nf.num_pieces nf) |> 
            List.filter ~f:(fun i -> not (Nf.has_piece nf i)) |>
            List.filter ~f:(fun i -> not (Hashtbl.mem t.requested i))
    in
    Strategy.next_requests l peers n  
    |> Deferred.List.iter ~how:`Parallel ~f:(request t nf) 
    |> don't_wait_for
  )

let setup_download t nf p =
  P.set_nf p nf;
  if (Nf.num_downloaded_pieces nf) > 0 then (
    info !"Pwp: sending bitfield %{Peer}"  p;
    Nf.downloaded nf |> P.send_bitfield p 
  );
  info !"Pwp: interested in %{Peer}" p;
  P.set_am_interested p true;
  info !"Pwp: interested in %{Peer}" p;
  info !"Pwp: unchoking %{Peer}" p;
  P.set_am_choking p false

let rec process t f =
  match%map Pipe.read t.rd with
  | `Eof -> assert false
  | `Ok (e, p) -> f t e p

let leave t p =
  info !"Pwp: %{P} has left (%d remaining)" p (Hashtbl.length t.peers);
  P.id p |> Hashtbl.remove t.peers

let without_nf t e p = 
  let open Peer in
  debug !"Pwp: event (no tinfo) %{P.event_to_string} from %{P}" e p;
  match e with 
  | Tinfo tinfo -> 
    info !"Pwp: got tinfo from %{P}" p;
    `Finished tinfo
  | Support_meta -> 
    P.request_meta p; 
    `Repeat ()
  | Bye -> 
    leave t p; 
    `Repeat ()
  | Join -> 
    `Repeat ()
  | _ -> 
    `Repeat ()

let with_nf nf t e p = 
  let open Peer in
  debug !"Pwp: event (tinfo) %{P.event_to_string} from %{P}" e p;
  match e with 
  | Bye -> 
    leave t p; 
    `Repeat ()
  | Join -> 
    setup_download t nf p; `Repeat ()
  | Piece i -> 
    begin
      match Hashtbl.find t.requested i with 
      | None -> 
        info !"Pwp: %{P} downloaded piece %d - but too late" p i;
        `Repeat ()
      | Some iv -> 
        debug !"Pwp: %{P} downloaded piece %d - filling ivar" p i;
        Ivar.fill iv (); 
        `Repeat ()
    end
  | Choke | Support_meta | Tinfo _ | Interested | Not_interested -> 
    `Repeat ()
  | Unchoke ->
    info !"Pwp: %{P} unchoked us" p;
    request_pieces t nf; 
    `Repeat ()
  | Bitfield | Have ->
    request_pieces t nf; 
    `Repeat ()

let init t p =
  info !"Pwp: %{Peer} added (#%d)" p (Hashtbl.length t.peers);
  P.start p;
  Pipe.transfer (Peer.event_reader p) t.wr ~f:(fun e -> (e, p)) 
  >>| fun () -> 
  Ok () 

let add_peer t p =
  let peer_id = P.id p in

  match Hashtbl.add t.peers ~key:peer_id ~data:p with 
  | `Ok  -> 
    init t p   
  | `Duplicate -> 
    (* we ignore all peers already connected, and ourselves. It may be the case
       that the calling layers try to add twice the same peer. For instance,
       the tracker can return our own address and we may try to connect to 
       ourselves  *)
    Error (Error.of_string "ignore peers (already added or ourselves)") 
    |> return

let close t = 
  (* Pipe.close t.wr;  *)
  match t.nf with 
  | None -> Deferred.unit  
  | Some nf -> Nf.close nf  

let status t = Status.{
    num_peers = Hashtbl.length t.peers; 
    num_downloaded_pieces = Option.value_exn t.nf |> Nf.num_downloaded_pieces
  } 

let start_tinfo t tinfo = 
  let n = t.info_hash |> Bt_hash.to_hex |> G.torrent_name 
          |> G.with_torrent_path in
  info "Pwp: saving meta-info to file %s" n; 
  Torrent.info_to_file n tinfo;
  let%bind nf = Nf.create t.info_hash tinfo in
  t.nf <- Some nf;
  setup_download t nf |> for_all_peers t;
  info "Pwp: start message handler 'with info' loop"; 
  Deferred.repeat_until_finished () (fun () -> process t (with_nf nf))

let start ?tinfo t = 
  (match tinfo with 
   | None -> (
       info !"Pwp: start message handler 'without info' loop"; 
       Deferred.repeat_until_finished () (fun () -> process t without_nf)
       >>= fun tinfo ->
       start_tinfo t tinfo)
   | Some tinfo -> 
     start_tinfo t tinfo
  ) |> don't_wait_for

let create info_hash = 
  let rd, wr = Pipe.create () in { 
    info_hash;
    peers = Hashtbl.Poly.create (); 
    nf = None;
    requested = Hashtbl.Poly.create ();
    rd;
    wr;
  }


