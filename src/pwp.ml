open Core
open Async
open Log.Global

module M = Message
module P = Peer
module Ps = Peer_state
module G = Global

type t_meta = {
  torrent : Torrent.info;
  file : File.t;
  pers : Pers.t;
  mutable num_requested : int;
}

type t_info = {
  mutable state : [`Requested | `Not_requested | `Valid  ];
  mutable piece : Piece.t Option.t
}

type t = {
  mutable has_meta : bool; (* convenience field *)
  mutable meta : t_meta Option.t;
  peers : (Peer_id.t, Ps.t) Hashtbl.t;
  info : t_info
}

let create ?meta () = 
  let info = {
    state = `Not_requested;
    piece = None;
  } in

  { 
    peers = Hashtbl.Poly.create (); 
    meta; 
    has_meta = Option.is_some meta; 
    info;
  }

(* This has to be called whenever a request is sent *)
let incr_requested t = t.num_requested <- t.num_requested + 1 

(* This has to be called whenever a request has been granted or cancelled *)
let decr_requested t = t.num_requested <- t.num_requested - 1

let for_all_peers t ~f = Hashtbl.iter t.peers ~f

let send_have_messages t i =
  let notify_if_doesn't_have i st =
    if not (Ps.has_piece st i) then (
      debug !"notify peer %{Ps} about piece %d" st i;
      let p = Ps.peer st in 
      M.Have i |> P.send p 
    ) in
  for_all_peers t ~f:(notify_if_doesn't_have i)

(* we always request all blocks from a piece to the same peer at the 
   same time *)
let request_all_blocks_from_piece (meta:t_meta) (st:Ps.t) (piece_i:int) : unit =
  (* debug !"requesting piece %d from peer %{}" piece_i st; *)
  incr_requested meta;
  File.set_piece_status meta.file piece_i `Requested; 
  Ps.add_pending st piece_i;
  let f ~index ~off ~len =
    M.Request(index, off, len) |> P.send st.peer 
  in 
  File.get_piece meta.file piece_i |> Piece.iter ~f 

(* try to request as many pieces as we can - there should be no more than
   G.max_pending_request *)
let try_request_pieces t =
  let meta = Option.value_exn t.meta in
  let n = G.max_pending_request - meta.num_requested in
  if n > 0 then 
    let f (piece_i, st) = 
      request_all_blocks_from_piece meta st piece_i 
    in
    Strategy.next_requests meta.file t.peers n |> List.iter ~f

let send_extended (st:Ps.t) kind me =
  let i = 
    match kind with 
    | `Handshake -> 0
    | `Metadata_ext -> Option.value_exn st.metadata_id  
  in
  M.Extended (i, Extension.to_bin me) |> P.send st.peer

let process_message (t:t) (st:Ps.t) (m:M.t) : unit =

  let meta = Option.value_exn t.meta in

  let process_block index bgn block =
    let piece = File.get_piece meta.file index in
    let len = String.length block in
    File.is_valid_piece_index meta.file index |> Peer.validate st.peer; 
    Piece.is_valid_block piece bgn len |> Peer.validate st.peer;
    match Piece.update piece bgn block with 
    | `Ok -> ()
    (* debug "got block - piece %d offset = %d" index bgn *)
    | `Hash_error -> 
      decr_requested meta;
      File.set_piece_status meta.file index `Not_requested;
      info !"hash error piece %d from %{Ps}" index st
    | `Downloaded ->
      (* debug !"got piece %d from %{} " index st; *)
      Peer.set_downloading st.peer;
      Ps.remove_pending st index;
      File.set_piece_status meta.file index `Downloaded;
      decr_requested meta;
      Pers.write_piece meta.pers piece;

      let one_percent = max (meta.torrent.Torrent.num_pieces / 100) 1 in
      if (File.num_downloaded_pieces meta.file % one_percent) = 0 then
        Print.printf "downloaded %d%%\n" (File.percent meta.file);

      (* notify peers that we have a piece they don't have. 
         TODO: we should do it too if we receive a bitfield *)
      send_have_messages t index 
  in

  let process_request index bgn length =
    let piece = File.get_piece meta.file index in
    File.is_valid_piece_index meta.file index |> P.validate st.peer;
    Piece.is_valid_block_request piece bgn length |> P.validate st.peer;
    File.has_piece meta.file index |> P.validate st.peer;
    if not (Ps.am_choking st) then (
      P.set_uploading st.peer;
      let piece = File.get_piece meta.file index in
      (* TODO: we could avoid a string allocation by using a substring 
         for the block in M.Piece *)
      let block = Piece.get_content piece ~off:bgn ~len:length in
      Message.Block (index, bgn, block)) |> P.send st.peer
  in

  let request_meta len =
    let open Extension in
    t.info.state <- `Requested;
    let f ~index ~off ~len =
      Request index |> send_extended st `Metadata_ext 
    in
    let p = Piece.create ~index:0 (Bt_hash.random ()) ~len in 
    Piece.iter ~f p
  in

  let process_extended id s =
    let open Extension in
    let kind = match id, st.metadata_id with
      | 0, _ -> `Handshake 
      | a, Some b when a = b -> `Metadata_ext
      | _ -> assert false
    in

    let em = Extension.of_bin kind s in
    info !"extended message %d %{Extension}" id em;
    match kind, em with 
    | `Handshake, Handshake [`Metadata (id, s)] ->   
      Ps.set_metadata_id st (Some id);
      Ps.set_metadata_size st (Some s);
      request_meta s 
    | `Handshake, Handshake [] -> ()

    | _ -> failwith (Extension.to_string em)  
  in
  match m with

  | M.KeepAlive -> ()

  | M.Choke -> 
    Ps.set_peer_choking st true;

  | M.Unchoke -> 
    Ps.set_peer_choking st false; 
    (* we try to request new pieces after any new event that can trigger
       availability of new pieces *)
    (* try_request_pieces t *)

  | M.Interested -> 
    Ps.set_peer_interested st true; 
    (* if not (Ps.am_choking st) then P.send st.peer Message.Unchoke *)

  | M.Not_interested -> 
    Ps.set_peer_interested st false;

  | M.Bitfield bits -> 
    (* TODO pourquoi pas une exception quand le bitset n'est pas 
       encore initialisé ? *)
    Ps.set_owned_pieces st bits; 
    try_request_pieces t 

  | M.Port port -> 
    info "received port %d" port;
    Ps.set_port st (Some port);
    if G.is_node () then (
      Addr.create (Peer.addr st.peer) port |> 
      Krpc.try_add |> Deferred.ignore |> don't_wait_for )

  | M.Extended (id, b) -> 
    process_extended id b 

  (* the following messages must have meta set *)

  | M.Have index -> 
    Option.is_some t.meta |> Peer.validate st.peer;
    Ps.set_owned_piece st index; 
    try_request_pieces t 

  | M.Request (index, bgn, length) -> 
    Option.is_some t.meta |> Peer.validate st.peer;
    if not (Ps.am_choking st) then process_request index bgn length

  | M.Block (index, bgn, block) -> 
    Option.is_some t.meta |> Peer.validate st.peer;
    process_block index bgn block; 
    try_request_pieces t

  | M.Cancel (index, bgn, length) ->
    Option.is_some t.meta |> Peer.validate st.peer;
    info "ignore cancel msg - Not yet implemented"

let cancel_requests meta st = 
  match meta with 
  | None -> ()
  | Some meta ->
    begin
      let f i = 
        decr_requested meta;
        Ps.remove_pending st i;
        File.set_piece_status meta.file i `Not_requested 
      in
      let s = Ps.pending st in 
      if not (Set.is_empty s) then (
        info !"cancelling %d requests from %{Ps}" (Set.length s) st 
      );
      Set.iter s ~f
    end

let remove_peer t pid : unit = 
  (* we can safely remove it, as we knows the connection has been cut. 
     TODO: is the fd properly disposed of? *)
  Hashtbl.remove t.peers pid

(* This is the main message processing loop. We consider two types of events.
   Timeout (idle peer), and message reception. *)
let rec wait_and_process_message t st =

  let result = function
    | `Ok m -> 
      process_message t st m; 
      `Repeat ()
    | `Eof ->  
      (* signal the deconnection of the peer *)
      cancel_requests t.meta st;
      info !"peer %{Ps} has left - remove it from peers" st; 
      Ps.id st |> remove_peer t;
      `Finished ()
  in
  let p = Ps.peer st in
  P.receive p |> Clock.with_timeout G.idle 
  >>| function
  | `Timeout -> 
    (* TODO decide what to do with these idle peers - keep using them but
       mark them as bad and give priority to other peers? now we just ignore
       them. *)
    info !"peer %{Ps} is slow - set idle" st;
    cancel_requests t.meta st;
    Ps.set_idle st true;
    `Finished ()
  | `Result r -> result r

let initiate_protocol t st : unit Deferred.t =

  (* 
  let meta = Option.value_exn t.meta in

  let p = Ps.peer st in
  (* we send this optional message if we own pieces of the file *)
  if (File.num_downloaded_pieces meta.file) > 0 then (
    info !"sending my bitfield to %{Peer}" p;
    M.Bitfield (File.bitfield meta.file) |> P.send p 
  );
  (* this should only be sent to peers we're interested in. To simplify, 
     we suppose we're intersted in all peers, but it should be changed TODO *)
  P.send p M.Interested; *)

  info "start message handler loop";
  Deferred.repeat_until_finished () (fun () -> wait_and_process_message t st)

let add_peer (t:t) peer =
  (* we ignore all peers already connected, and ourselves. It may be the case
     that the calling layers try to add twice the same peer. For instance,
     the tracker can return our own address and we may try to connect to 
     ourselves  *)

  let st = Ps.create peer in
  let peer_id = Ps.id st in

  (match t.meta with 
   | Some meta -> Ps.init_size_owned_pieces st meta.torrent.Torrent.num_pieces
   | None -> ());

  match Hashtbl.add t.peers ~key:peer_id ~data:st with 
  | `Ok  -> initiate_protocol t st |> Deferred.ok
  | `Duplicate -> 
    Error (Error.of_string "ignore peers (already added or ourselves)") 
    |> return


