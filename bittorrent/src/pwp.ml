open Core
open Async
open Log.Global

module M = Message
module P = Peer
module G = Global
module Nf = Network_file

type table = (int, Peer.t list) Hashtbl.t
(* 
let add_peer_pieces table p =
  let bf = Peer.bitfield p |> (Bitfield.to_list 4444) in
  let upd = function 
    | None -> [p]
    | Some l -> p :: l 
  in
  List.iter bf ~f:(fun i -> Hashtbl.update table i ~f:upd)

 *)
(*

  pour chaque piece que je n'ai pas encore :
  liste des peers non choking qui l'ont 

  piece 1 -> [p0]
        2 -> [p1; p2]
        3 -> [p0]

initialement : vide

mise a jour quand :
 je reçois une piece : on vire une ligne
 un peer part ou choke : on l'enleve de partout
 un peer unchoke
 un peer envoie un bitfield ou have

on garde cette liste ordonnée par rarité first

*)

type t = {
  info_hash : Bt_hash.t;
  mutable nf : Nf.t Option.t;
  mutable peers : P.t Set.Poly.t;
  requested : (int, unit Ivar.t) Hashtbl.t;
  wr : (Peer.event * Peer.t) Pipe.Writer.t; 
  rd : (Peer.event * Peer.t) Pipe.Reader.t;
  mutable available : table;
}

let for_all_peers t ~f = Set.iter t.peers ~f

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
    let peers = Set.to_list t.peers in
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
  Torrent.info_to_file n tinfo;
  let%bind nf = Nf.create t.info_hash tinfo in
  t.nf <- Some nf;
  setup_download t nf |> for_all_peers t;
  info "Pwp: start message handler 'with info' loop"; 
  Deferred.repeat_until_finished () (fun () -> process t (with_nf nf))

let start t tinfo = 
  start_opt t tinfo |> Deferred.ignore

let close t = 
  info !"Pwp: closing %{Bt_hash.to_string_hum}" t.info_hash;
  Set.to_list t.peers |> Deferred.List.iter ~f:Peer.close
  >>= fun () ->
  Pipe.close t.wr; 
  match t.nf with 
  | None -> Deferred.unit  
  | Some nf -> Nf.close nf  

let create info_hash = 
  info !"Pwp: create %{Bt_hash.to_string_hum}" info_hash;
  let rd, wr = Pipe.create () in { 
    info_hash;
    peers = Set.Poly.empty; (* TODO use a set based on peer-id *)
    nf = None;
    requested = Hashtbl.Poly.create ();
    rd;
    wr;
    available = Hashtbl.Poly.create ();
  }


