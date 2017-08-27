open Core
open Async
open Blog

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

  mutable sm : Shared_meta.t option;
  mutable sent_bitfield : bool;

  mutable received_bitfield : bool;

  block_consumer : Block_consumer.t;
  block_producer : Block_producer.t;
} 

let id t = t.id

let create info_hash id peer sm event_wr ~dht ~extension =
  info !"Peer: %{Peer_id.to_string_hum} created" id;
  let block_rd, block_wr = Pipe.create () in
  Pipe.set_size_budget block_wr 3;
  let bitfield = Bitfield.empty G.max_num_pieces in
  let peer_ext = 
    match extension with
    | true ->
      let peer_ext_event_rd, peer_ext_event_wr = Pipe.create () in
      Some ((Peer_ext.create info_hash peer peer_ext_event_wr sm), peer_ext_event_rd)
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
    sm;
    sent_bitfield = false;
    received_bitfield = false;
    peer_ext;
    block_consumer = Block_consumer.create block_rd peer;
    block_producer = Block_producer.create block_wr bitfield;
  }

let is_interesting t = 
  let bf = t.bitfield in
  match t.sm with 
  | None -> false
  | Some sm ->
    let downloaded = Shared_meta.downloaded sm in 
    let num_pieces = Shared_meta.num_pieces sm in
    not (Bitfield.is_subset num_pieces bf downloaded)

let push_event t e = Pipe.write_without_pushback_if_open t.event_wr (e, t)

let validate t b = 
  let exception Incorrect_behavior in
  if not b then raise Incorrect_behavior

let to_string t = t.id |> Peer_id.to_string_hum  

let is_or_not b = if b then "" else "not"

let send_bitfield t sm = 
  assert (not t.sent_bitfield);
  if Shared_meta.has_any_piece sm then (
    Message.Bitfield (Shared_meta.downloaded sm) |> Peer_comm.send t.peer;
    t.sent_bitfield <- true
  ) 

let set_am_interested t b = 
  assert (t.am_interested = (not b));
  info !"Peer %{}: I am %{is_or_not} interested" t b;
  t.am_interested <- b;
  (if b then Message.Interested else Message.Not_interested) |> Peer_comm.send t.peer

let set_am_choking t b = 
  assert (t.am_choking = (not b));
  info !"Peer %{}: I am %{is_or_not} choking" t b;
  t.am_choking <- b;
  (if b then Message.Choke else Message.Unchoke) |> Peer_comm.send t.peer

let notify t i = 
  if not (Bitfield.get t.bitfield i) then
    Peer_comm.send t.peer (Message.Have i);
  if t.am_interested && not (is_interesting t) then
    set_am_interested t false

let clear_requests t sm =
  let f i =
    debug !"Peer %{}: clear requests %d" t i;
    Shared_meta.remove_requested sm i
  in
  List.iter (Block_consumer.pending_requests t.block_consumer) ~f

let request_meta_info t =  
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

  let process_block t sm index bgn block =
    let piece = Shared_meta.get_piece sm index in
    let len = String.length block in
    Shared_meta.is_valid_piece_index sm index |> validate t; 
    let b = Block.{ piece = index; off = bgn; len} in
    Block_consumer.notify t.block_consumer b;
    match Piece.update piece bgn block with 
    | `Ok -> ()
    | `Hash_error -> 
      info !"Peer %{}: hash error piece %d" t index
    | `Downloaded ->
      info !"Peer %{}: downloaded piece %d" t index; 
      Shared_meta.set_downloaded sm index;
      Shared_meta.write_piece sm index;
      Shared_meta.remove_requested sm index;
      push_event t (Piece index)

  let process_request t sm block =
    let Block.{ piece; off; len } = block in
    validate t t.peer_interested;
    validate t (not t.am_choking);
    let piece_ = Shared_meta.get_piece sm piece in
    Shared_meta.is_valid_piece_index sm piece |> validate t;
    Piece.is_valid_block_request piece_ ~off ~len |> validate t;
    Shared_meta.is_downloaded sm piece |> validate t;
    let block_content = Piece.get_content piece_ ~off ~len in
    Message.Block (piece, off, block_content) |> Peer_comm.send t.peer

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
      let f sm =  
        if not (Shared_meta.is_downloaded sm i)  && not t.am_interested then 
          set_am_interested t true
      in 
      Option.iter t.sm ~f

    | Message.Request block -> 
      assert (Option.is_some t.sm);
      if not t.am_choking then 
        process_request t (Option.value_exn t.sm) block

    | Message.Block (i, bgn, block) -> 
      assert (Option.is_some t.sm);
      process_block t (Option.value_exn t.sm) i bgn block

    | Message.Cancel block -> 
      assert (Option.is_some t.sm);
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
    Peer_comm.receive t.peer |> Clock.with_timeout G.keep_alive
    >>| function 
    | `Timeout -> 
      push_event t Bye;
      `Finished ()
    | `Result r -> result r 

  let start t = 
    debug !"Peer %{}: start message handler loop" t; 
    Deferred.repeat_until_finished () (wait_and_process_message t)
end

let start_sm t sm = 
  send_bitfield t sm;
  Block_consumer.start t.block_consumer;
  Block_producer.start t.block_producer sm;
  if t.received_bitfield && (is_interesting t) then (
    validate t t.peer_choking;
    set_am_interested t true
  )

let set_shared_meta t sm =
  assert (Option.is_none t.sm);
  t.sm <- Some sm;
  start_sm t sm

let start t = 
  let f (peer_ext, peer_ext_event_rd) = 
    Peer_ext.send_handshake peer_ext;
    Pipe.transfer peer_ext_event_rd t.event_wr ~f:(fun e -> (e,t))
    |> don't_wait_for
  in
  Option.iter t.peer_ext ~f; 
  Option.iter t.sm ~f:(start_sm t);
  Message_loop.start t 

let close t = 
  debug !"Peer %{}: we close this peer." t;
  Option.iter t.sm ~f:(clear_requests t);
  Block_producer.close t.block_producer;
  Option.iter t.peer_ext ~f:(fun (pe, _) -> Peer_ext.close pe); 
  Peer_comm.close t.peer
