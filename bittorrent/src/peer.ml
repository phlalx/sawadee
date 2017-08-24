open Core
open Async
open Blog

module Nf = Network_file
module Pc = Peer_comm
module G = Global

type t = {
  peer : Peer_comm.t;
  info_hash : Bt_hash.t;

  id : Peer_id.t;  (* TODO these two fields can go in peer_comm *)
  dht : bool;
  peer_ext : (Peer_ext.t * Pevent.t Pipe.Reader.t) option;

  mutable peer_choking : bool; 
  mutable peer_interested : bool;
  mutable am_choking : bool;
  mutable am_interested : bool; 
  mutable received_port : bool ;
  bitfield : Bitfield.t;

  event_wr : (Pevent.t * t) Pipe.Writer.t; 

  mutable nf : Network_file.t option;
  mutable sent_bitfield : bool;

  mutable received_bitfield : bool;

  block_consumer : Block_consumer.t;
  block_producer : Block_producer.t;
} 

let id t = t.id

let create info_hash id peer nf event_wr ~dht ~extension =
  info !"Peer: %{Peer_id.to_string_hum} created" id;
  let block_rd, block_wr = Pipe.create () in
  Pipe.set_size_budget block_wr 3;
  let bitfield = Bitfield.empty G.max_num_pieces in
  let peer_ext = 
    match extension with
    | true ->
      let peer_ext_event_rd, peer_ext_event_wr = Pipe.create () in
      Some ((Peer_ext.create info_hash peer peer_ext_event_wr nf), peer_ext_event_rd)
    | false -> None
  in
  {
    info_hash;
    id;
    peer;
    peer_interested = false; 
    peer_choking = true; 
    am_interested = false;
    am_choking = true;
    received_port = false;
    bitfield;
    event_wr;
    dht;
    nf;
    sent_bitfield = false;
    received_bitfield = false;
    peer_ext;
    block_consumer = Block_consumer.create block_rd peer;
    block_producer = Block_producer.create block_wr bitfield;
  }

let is_interesting t = 
  let bf = t.bitfield in
  match t.nf with 
  | None -> false
  | Some nf ->
    let downloaded = Nf.downloaded nf in 
    let num_pieces = Nf.num_pieces nf in
    not (Bitfield.is_subset num_pieces bf downloaded)

let push_event t e = Pipe.write_without_pushback_if_open t.event_wr (e, t)

let validate t b = 
  let exception Incorrect_behavior in
  if not b then raise Incorrect_behavior

let to_string t = t.id |> Peer_id.to_string_hum  

let is_or_not b = if b then "" else "not"

let send_bitfield t nf = 
  assert (not t.sent_bitfield);
  if Nf.has_any_piece nf then (
    Message.Bitfield (Nf.downloaded nf) |> Pc.send t.peer;
    t.sent_bitfield <- true
  ) 

let set_am_interested t b = 
  assert (t.am_interested = (not b));
  info !"Peer %{}: I am %{is_or_not} interested" t b;
  t.am_interested <- b;
  (if b then Message.Interested else Message.Not_interested) |> Pc.send t.peer

let set_am_choking t b = 
  assert (t.am_choking = (not b));
  info !"Peer %{}: I am %{is_or_not} choking" t b;
  t.am_choking <- b;
  (if b then Message.Choke else Message.Unchoke) |> Pc.send t.peer

let notify t i = 
  if not (Bitfield.get t.bitfield i) then
    Pc.send t.peer (Message.Have i);
  if t.am_interested && not (is_interesting t) then
    set_am_interested t false

let clear_requests t nf =
  let f i =
    debug !"Peer %{}: clear requests %d" t i;
    Nf.remove_requested nf i
  in
  List.iter (Block_consumer.pending_requests t.block_consumer) ~f

let request_meta t =  
  match t.peer_ext with
  | None -> assert false
  | Some (pe, _) -> Peer_ext.request_meta pe

let status t = Status.{
    dl = Peer_comm.downloaded t.peer; 
    ul = Peer_comm.uploaded t.peer;
    dl_speed = Peer_comm.download_speed t.peer; 
    ul_speed = Peer_comm.upload_speed t.peer;
    client = Peer_id.client t.id;
    addr = Peer_comm.addr t.peer;
  }

module Message_loop : sig 

  val start : t -> unit Deferred.t

end = 
struct

  let process_block t nf index bgn block =
    let piece = Network_file.get_piece nf index in
    let len = String.length block in
    Network_file.is_valid_piece_index nf index |> validate t; 
    let b = Block.{ piece = index; off = bgn; len} in
    Block_consumer.notify t.block_consumer b;
    match Piece.update piece bgn block with 
    | `Ok -> ()
    | `Hash_error -> 
      info !"Peer %{}: hash error piece %d" t index
    | `Downloaded ->
      info !"Peer %{}: downloaded piece %d" t index; 
      Nf.set_downloaded nf index;
      Nf.write_piece nf index;
      Nf.remove_requested nf index;
      push_event t (Piece index)

  let process_request t nf block =
    let Block.{ piece; off; len } = block in
    validate t t.peer_interested;
    validate t (not t.am_choking);
    let piece_ = Network_file.get_piece nf piece in
    Network_file.is_valid_piece_index nf piece |> validate t;
    Piece.is_valid_block_request piece_ ~off ~len |> validate t;
    Network_file.is_downloaded nf piece |> validate t;
    let block_content = Piece.get_content piece_ ~off ~len in
    Message.Block (piece, off, block_content) |> Pc.send t.peer

  let process_message t m : unit =
    debug !"Peer %{}: received %{Message}" t m;
    match m with

    | Message.KeepAlive -> ()

    | Message.Choke -> 
      t.peer_choking <- true;
      Block_producer.set_choking t.block_producer true

    | Message.Unchoke -> 
      t.peer_choking <- false;
      Block_producer.set_choking t.block_producer false

    | Message.Interested -> 
      t.peer_interested <- true;
      if t.am_choking then
        set_am_choking t false 

    | Message.Not_interested -> 
      t.peer_interested <- false;

    | Message.Bitfield bits -> 
      info !"Peer %{}: received bitfield (%d pieces)" t (Bitfield.card bits);
      Bitfield.copy ~src:bits ~dst:t.bitfield; 
      validate t (not t.received_bitfield);
      t.received_bitfield <- true;
      if is_interesting t then ( 
        validate t t.peer_choking;
        set_am_interested t true
      )

    | Message.Port port -> 
      not t.received_port |> validate t;
      t.received_port <- true;
      let f dht = 
        Addr.create (Peer_comm.addr t.peer) port |> 
        Dht.try_add dht |> Deferred.ignore |> don't_wait_for 
      in
      Option.iter (G.dht ()) ~f 

    | Message.Extended (id, b) -> (
        match t.peer_ext with
        | None -> validate t false
        | Some (pe, _) ->  
          let em = Extension.of_bin b in 
          Peer_ext.process_extended pe id em)

    | Message.Have i -> 
      Bitfield.set t.bitfield i true;
      let f nf =  
        if not (Nf.is_downloaded nf i)  && not t.am_interested then 
          set_am_interested t true
      in 
      Option.iter t.nf ~f

    | Message.Request block -> 
      assert (Option.is_some t.nf);
      if not t.am_choking then 
        process_request t (Option.value_exn t.nf) block

    | Message.Block (i, bgn, block) -> 
      assert (Option.is_some t.nf);
      process_block t (Option.value_exn t.nf) i bgn block

    | Message.Cancel block -> 
      assert (Option.is_some t.nf);
      info !"Peer %{}: not implemented yet" t

  let rec wait_and_process_message t () =

    let result = function
      | `Ok m -> 
        process_message t m; 
        `Repeat ()
      | `Eof -> 
        push_event t Bye; 
        `Finished ()
    in
    Pc.receive t.peer |> Clock.with_timeout G.keep_alive
    >>| function 
    | `Timeout -> 
      push_event t Bye;
      `Finished ()
    | `Result r -> result r 

  let start t = 
    debug !"Peer %{}: start message handler loop" t; 
    Deferred.repeat_until_finished () (wait_and_process_message t)
end

let start_nf t nf = 
  send_bitfield t nf;
  Block_consumer.start t.block_consumer;
  Block_producer.start t.block_producer nf;
  if t.received_bitfield && (is_interesting t) then (
    validate t t.peer_choking;
    set_am_interested t true
  )

let set_nf t nf =
  assert (Option.is_none t.nf);
  t.nf <- Some nf;
  start_nf t nf

let start t = 
  let f (peer_ext, peer_ext_event_rd) = 
    Peer_ext.send_handshake peer_ext;
    Pipe.transfer peer_ext_event_rd t.event_wr ~f:(fun e -> (e,t))
    |> don't_wait_for
  in
  Option.iter t.peer_ext ~f; 
  Option.iter t.nf ~f:(start_nf t);
  Message_loop.start t 

let close t = 
  debug !"Peer %{}: we close this peer." t;
  Option.iter t.nf ~f:(clear_requests t);
  Block_producer.close t.block_producer;
  Option.iter t.peer_ext ~f:(fun (pe, _) -> Peer_ext.close pe); 
  Peer_comm.close t.peer
