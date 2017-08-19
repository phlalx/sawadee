open Core
open Async
open Log.Global

module Nf = Network_file
module Pc = Peer_comm
module G = Global

type meta = {
  id : Extension.id;
  total_length : int;
  num_block : int;
  data : string;
  received : bool Array.t
}

type t = {
  peer : Peer_comm.t;

  id : Peer_id.t;  (* TODO these three fields can go in peer_comm *)
  dht : bool;

  mutable peer_choking : bool; 
  mutable peer_interested : bool;
  mutable am_choking : bool;
  mutable am_interested : bool; 
  mutable received_port : bool ;
  bitfield : Bitfield.t;

  event_wr : (Pevent.t * t) Pipe.Writer.t; 
  peer_ext : (Peer_ext.t * Pevent.t Pipe.Reader.t) option;

  mutable nf : Network_file.t option;
  mutable sent_bitfield : bool;
  block_wr : Block.t Pipe.Writer.t;
  block_rd : Block.t Pipe.Reader.t;
  mutable pending : Block.t Set.Poly.t;
  mutable received_bitfield : bool;
  can_consume : unit Condition.t;
  can_produce : unit Condition.t;
} [@@deriving fields]


(* TODO add validation check *)
let has_piece t i = Bitfield.get t.bitfield i

let next_requests t nf = 

  let f i = 
    not (Nf.is_downloaded nf i) && not (Nf.is_requested nf i) && (has_piece t i)
  in

  List.range 0 (Nf.num_pieces nf) |> List.filter ~f

let can_produce t nf =
  match next_requests t nf with
  | h :: _ when not t.peer_choking (* && t.am_interested *) -> Some h
  | _ -> None

let is_interesting t nf = 
  let downloaded = Nf.downloaded nf in 
  let num_pieces = Nf.num_pieces nf in
  let bf = t.bitfield in
  not (Bitfield.is_subset num_pieces bf downloaded)

let create id peer nf event_wr ~dht ~extension =
  info !"Peer: %{Peer_id.to_string_hum} created" id;
  let block_rd, block_wr = Pipe.create () in
  Pipe.set_size_budget block_wr 3;
  let peer_ext = 
    if extension then
      let peer_ext_event_rd, peer_ext_event_wr = Pipe.create () in
      Some ((Peer_ext.create peer peer_ext_event_wr), peer_ext_event_rd)
    else 
      None
  in
  {
    id;
    peer;
    peer_interested = false; 
    peer_choking = true; 
    am_interested = false;
    am_choking = true;
    received_port = false;
    bitfield = Bitfield.empty G.max_num_pieces;
    event_wr;
    dht;
    nf;
    sent_bitfield = false;
    block_rd;
    block_wr;
    pending = Set.Poly.empty;
    can_consume = Condition.create ();
    can_produce = Condition.create ();
    received_bitfield = false;
    peer_ext;
  }

let push_event t e = Pipe.write_without_pushback_if_open t.event_wr (e, t)

exception Incorrect_behavior

let validate t b = if not b then raise Incorrect_behavior

let id t = t.id

let to_string t = id t |> Peer_id.to_string_hum  

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


module Message_loop : sig 

  val start : t -> unit Deferred.t

end = 
struct

  let set_peer_interested t b = 
    info !"Peer %{}: is %{is_or_not} interested" t b;
    t.peer_interested <- b

  let set_peer_choking t b = 
    info !"Peer %{}: is %{is_or_not} choking" t b;
    t.peer_choking <- b

  let process_block t nf index bgn block =
    let piece = Network_file.get_piece nf index in
    let len = String.length block in
    Network_file.is_valid_piece_index nf index |> validate t; 
    let b = Block.{ piece = index; off = bgn; len} in
    Set.mem t.pending b |> validate t;
    t.pending <- Set.remove t.pending b;
    if (Set.length t.pending) < G.max_pending_request then ( 
      info !"Peer %{}: wake-up block consumer" t;
      Condition.signal t.can_consume ()
    );
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
      set_peer_choking t true

    | Message.Unchoke -> 
      set_peer_choking t false; (
        match t.nf with
        | None -> validate t false
        | Some nf -> 
          debug !"Peer %{}: wake up block producer" t;
          Condition.signal t.can_produce ())

    | Message.Interested -> 
      set_peer_interested t true;
      set_am_choking t false 

    | Message.Not_interested -> 
      set_peer_interested t false

    | Message.Bitfield bits -> 
      info !"Peer %{}: received bitfield (%d pieces)" t (Bitfield.card bits);
      Bitfield.copy ~src:bits ~dst:t.bitfield; 
      validate t (not t.received_bitfield);
      t.received_bitfield <- true;
      let f nf = 
        if is_interesting t nf then ( 
          validate t t.peer_choking;
          set_am_interested t true
        )
      in Option.iter t.nf ~f

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
          if Option.is_none t.nf then ( (* TODO should work without this condition *)
            let em = Extension.of_bin b in 
            Peer_ext.process_extended pe id em))

    | Message.Have i -> 
      Bitfield.set t.bitfield i true;
      let f nf =  
        if not (Nf.is_downloaded nf i)  && not t.am_interested then 
          set_am_interested t true
      in 
      Option.iter t.nf ~f

    | Message.Request block -> 
      assert (Option.is_some t.nf);
      if not (am_choking t) then 
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

module Block_consumer = 
struct

  let process t b =
    debug !"Peer: requesting block %{Block}" b;
    Message.Request b |> Pc.send t.peer;
    t.pending <- Set.add t.pending b

  let consume t () = 
    debug !"Peer %{}: trying to consume a block (%d blocks in pipe)" t (Pipe.length t.block_rd); 
    if (Set.length t.pending) < G.max_pending_request then (
      match%bind Pipe.read t.block_rd with
      | `Eof -> `Finished () |> return
      | `Ok b -> process t b; `Repeat () |> return
    ) else (
      debug !"Peer %{}: block consumer goes to sleep" t; 
      Condition.wait t.can_consume >>| fun () -> `Repeat ()
    )

  let start t = 
    info !"Peer %{}: start block consumer" t; 
    Deferred.repeat_until_finished () (consume t) |> don't_wait_for 

end 

module Block_producer =
struct 

  let produce_blocks' t nf i = 
    Nf.add_requested nf i;
    let l = Network_file.get_piece nf i |> Piece.blocks in
    debug !"Peer %{}: producing %d new blocks for piece %d (pipe contains\
            %d blocks)" t (List.length l) i (Pipe.length t.block_wr); 
    Deferred.List.iter l ~f:(Pipe.write_if_open t.block_wr)
    >>| fun () -> `Repeat ()

  let produce_blocks t nf i =
    if Pipe.is_closed t.block_wr then
      `Finished () |> return
    else 
      produce_blocks' t nf i

  let produce t nf () =
    match can_produce t nf with
    | None -> 
      debug !"Peer %{}: block producer goes to sleep" t;
      (* TODO send not interested *)
      Condition.wait t.can_produce >>| fun () -> `Repeat ()
    | Some i -> 
      produce_blocks t nf i

  let start t nf = 
    info !"Peer %{}: start block producer" t; 
    Deferred.repeat_until_finished () (produce t nf) |> don't_wait_for

end

let start_nf t nf = 
  send_bitfield t nf;
  Block_consumer.start t;
  Block_producer.start t nf;
  if t.received_bitfield && (is_interesting t nf) then (
    validate t t.peer_choking;
    set_am_interested t true
  )

let set_nf t nf =
  assert (Option.is_none t.nf);
  t.nf <- Some nf;
  start_nf t nf

let clear_requests t nf =
  let f i =
    debug !"Peer %{}: clear requests %d" t i;
    Nf.remove_requested nf i
  in
  Set.Poly.map t.pending ~f:(fun { Block.piece } -> piece ) |> Set.iter  ~f

let close t = 
  debug !"Peer %{}: we close this peer." t;
  Option.iter t.nf ~f:(clear_requests t);
  Pipe.close t.block_wr;
  Option.iter t.peer_ext ~f:(fun (pe, _) -> Peer_ext.close pe); 
  Peer_comm.close t.peer

let start t = 
  Option.iter t.nf ~f:(start_nf t);
  let f (_, peer_ext_event_rd) = 
    Pipe.transfer peer_ext_event_rd t.event_wr ~f:(fun e -> (e,t))
    |> don't_wait_for
  in
  Option.iter t.peer_ext ~f; 
  Message_loop.start t 

let send_have t i = Message.Have i |> Pc.send t.peer 

let request_meta t =  
  match t.peer_ext with
  | None -> assert false
  | Some (pe, _) -> Peer_ext.request_meta pe

let status t = Status.{
    dl = Peer_comm.downloaded t.peer; 
    ul = Peer_comm.uploaded t.peer;
    dl_speed = Peer_comm.download_speed t.peer; 
    ul_speed = Peer_comm.upload_speed t.peer;
    client = Peer_id.client (id t);
    addr = Peer_comm.addr t.peer;
  }



