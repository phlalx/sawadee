open Core
open Async
open Log.Global

module Nf = Network_file
module M = Message
module P = Peer_comm
module G = Global
module B = Bencode_ext

type event = 
  | Support_meta
  | Tinfo of Torrent.info
  | Choke 
  | Unchoke
  | Interested
  | Not_interested 
  | Have of int
  | Bitfield 
  | Piece of int
[@@deriving sexp]

type meta = {
  id : int;
  total_length : int;
  num_block : int;
  data : string;
  received : bool Array.t
}

type block = Piece.block

type t = {
  id : Peer_id.t;
  peer : Peer_comm.t;
  extension : bool;
  dht : bool;
  mutable peer_choking : bool; 
  mutable peer_interested : bool;
  mutable am_choking : bool;
  mutable am_interested : bool; 
  mutable sent_port : bool ;
  bitfield : Bitfield.t;
  event_wr : event Pipe.Writer.t; 
  event_rd : event Pipe.Reader.t;
  mutable meta : meta option; (* protocol id and metadata size *)
  mutable nf : Network_file.t option;
  mutable sent_bitfield : bool;
  conn_stat : Conn_stat.t;
  block_wr : block Pipe.Writer.t;
  block_rd : block Pipe.Reader.t;
  mutable pending : block Set.Poly.t;
  few_pending : unit Condition.t;
  received_bitfield : bool;
  can_produce : unit Condition.t
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

let is_interesting t = 
  let nf = Option.value_exn t.nf in
  let downloaded = Nf.downloaded nf in 
  let num_pieces = Nf.num_pieces nf in
  let bf = t.bitfield in
  not (Bitfield.is_subset num_pieces bf downloaded)

let create id peer ~dht ~extension =
  let event_rd, event_wr = Pipe.create () in 
  let block_rd, block_wr = Pipe.create () in
  Pipe.set_size_budget block_wr 3;
  {
    id;
    peer;
    peer_interested = false; 
    peer_choking = true; 
    am_interested = false;
    am_choking = true;
    sent_port = false;
    bitfield = Bitfield.empty G.max_num_pieces;
    event_rd;
    event_wr;
    extension;
    dht;
    meta = None;
    nf = None;
    sent_bitfield = false;
    conn_stat = Conn_stat.create ();
    block_rd;
    block_wr;
    pending = Set.Poly.empty;
    few_pending = Condition.create ();
    can_produce = Condition.create ();
    received_bitfield = false;
  }

let push_event t e = Pipe.write_without_pushback_if_open t.event_wr e

let event_to_string e = Sexp.to_string (sexp_of_event e)

exception Incorrect_behavior

let validate t b = if not b then raise Incorrect_behavior

let id t = t.id

let to_string t = id t |> Peer_id.to_string_hum  

let bitfield t = t.bitfield

let is_or_not b = if b then "" else "not"


let set_am_interested t b = 
  assert (t.am_interested = (not b));
  debug !"Peer %{}: I am %{is_or_not} interested" t b;
  t.am_interested <- b;
  (if b then M.Interested else M.Not_interested) |> P.send t.peer

let set_am_choking t b = 
  assert (t.am_choking = (not b));
  debug !"Peer %{}: I am %{is_or_not} choking" t b;
  t.am_choking <- b;
  (if b then M.Choke else M.Unchoke) |> P.send t.peer

module Message_loop : sig 

  val start : t -> unit Deferred.t

end = 
struct

  let set_peer_interested t b = 
    debug !"Peer %{}: is %{is_or_not} interested" t b;
    t.peer_interested <- b

  let set_peer_choking t b = 
    debug !"Peer %{}: is %{is_or_not} choking" t b;
    t.peer_choking <- b

  let process_block t nf index bgn block =
    let piece = Network_file.get_piece nf index in
    let len = String.length block in
    Network_file.is_valid_piece_index nf index |> validate t; 
    let b = Piece.{ b_index = index; off = bgn; len} in
    Set.mem t.pending b |> validate t;
    t.pending <- Set.remove t.pending b;
    if (Set.length t.pending) < G.max_pending_request then ( 
      debug !"Peer %{}: signal slots availabe" t;
      Condition.signal t.few_pending ()
    );
    match Piece.update piece bgn block with 
    | `Ok -> 
      info !"Peer %{}: got block - piece %d offset = %d" t index bgn
    | `Hash_error -> 
      debug !"Peer %{}: hash error piece %d" t index
    | `Downloaded ->
      info !"Peer %{}: got piece %d" t index; 
      P.set_downloading t.peer;
      Nf.set_downloaded nf index;
      Nf.write_piece nf index;
      Nf.remove_requested nf index;
      push_event t (Piece index)

  let update_data t i s = 
    debug !"Peer %{}: received piece %d" t i; 
    let { received ; total_length; data } = Option.value_exn t.meta in
    received.(i) <- true;
    let off = i * G.meta_block_size in
    let len = min (total_length - i * G.meta_block_size) G.meta_block_size in
    String.blit ~src:s ~dst:data ~src_pos:0 ~dst_pos:off ~len;
    match Array.fold ~init:true ~f:(fun acc b -> acc && b) received  with
    | true ->
      debug !"Peer %{}: data complete" t; 

      let tinfo = Torrent.info_of_string data in
      Tinfo tinfo |> push_event t
    | false -> ()

  let process_extended t id s =
    validate t (id = 0);
    let em = Extension.of_bin s in 
    debug !"Peer %{}: process ext. message %{Extension}" t em; 
    match em with 
    | Extension.Handshake [`Metadata (id, total_length)] ->
      let num_block = (total_length + G.meta_block_size - 1) / 
                      G.meta_block_size in
      let data = String.create total_length in
      let received = Array.create num_block false in
      t.meta <- Some {id; total_length; num_block; data; received};
      push_event t Support_meta
    | Extension.Data (i, s) -> update_data t i s  
    | _ -> info !"Peer %{}: not implemented" t

  let process_request t nf index bgn length =
    let piece = Network_file.get_piece nf index in
    Network_file.is_valid_piece_index nf index |> validate t;
    Piece.is_valid_block_request piece bgn length |> validate t;
    Network_file.is_downloaded nf index |> validate t;
    P.set_uploading t.peer; (* TODO pass this in this module *)
    let piece = Network_file.get_piece nf index in
    (* TODO: we could avoid a string allocation by using a substring 
       for the block in M.Piece *)

    let block = Piece.get_content piece ~off:bgn ~len:length in
    Conn_stat.incr_ul t.conn_stat length;
    Message.Block (index, bgn, block) |> P.send t.peer


  let signal_can_produce t =
    match t.nf with
    | None -> ()
    | Some nf ->  
      match can_produce t nf with
      | Some _ -> 
        debug !"Peer %{}: signaling can_produce" t;
        Condition.signal t.can_produce ()
      | None -> ()

  let process_message t m : unit =
    debug !"Peer %{}: received %{Message}" t m;
    match m with

    | M.KeepAlive -> ()

    | M.Choke -> 
      set_peer_choking t true;
      push_event t Choke

    | M.Unchoke -> 
      signal_can_produce t;
      set_peer_choking t false; 
      push_event t Unchoke

    | M.Interested -> 
      set_peer_interested t true; 
      push_event t Interested

    | M.Not_interested -> 
      set_peer_interested t false;
      push_event t Not_interested

    | M.Bitfield bits -> 
      info !"Peer %{}: received bitfield (%d pieces)" t (Bitfield.card bits);
      Bitfield.copy ~src:bits ~dst:t.bitfield; 
      signal_can_produce t;
      validate t (not t.received_bitfield);
      if Option.is_some t.nf then
        push_event t Bitfield

    | M.Port port -> 
      debug !"Peer %{}: received port %d" t port;
      not t.sent_port |> validate t;
      let f dht = 
        Addr.create (Peer_comm.addr t.peer) port |> 
        Dht.try_add dht |> Deferred.ignore |> don't_wait_for 
      in
      Option.iter (G.dht ()) ~f 

    | M.Extended (id, b) -> 
      if Option.is_none t.nf then 
        process_extended t id b 

    | M.Have index -> 
      debug !"Peer %{}: received have %d" t index;
      Bitfield.set t.bitfield index true;
      signal_can_produce t;
      if Option.is_some t.nf then
        push_event t (Have index)

    | M.Request (index, bgn, length) -> 
      info !"Peer %{}: *** peer requests %d ***" t index;
      assert (Option.is_some t.nf);
      if not (am_choking t) then 
        process_request t (Option.value_exn t.nf) index bgn length

    | M.Block (index, bgn, block) -> 
      assert (Option.is_some t.nf);
      Conn_stat.incr_dl t.conn_stat (String.length block);
      process_block t (Option.value_exn t.nf) index bgn block

    | M.Cancel (index, bgn, length) -> 
      assert (Option.is_some t.nf);
      info !"Peer %{}: not implemented yet" t

  let clear_requests t nf =
    let f i =
      debug !"Peer %{}: clear requests %d" t i;
      Nf.remove_requested nf i
    in
    Set.Poly.map t.pending ~f:(fun { Piece.b_index } -> b_index ) |> Set.iter  ~f

  let rec wait_and_process_message t () =

    let result = function
      | `Ok m -> process_message t m; `Repeat ()
      | `Eof -> 
        Option.iter t.nf ~f:(clear_requests t);
        debug !"Peer %{}: Eof" t; 
        `Finished ()
    in
    P.receive t.peer |> Clock.with_timeout G.keep_alive
    >>| function 
    | `Timeout -> 
      Option.iter t.nf ~f:(clear_requests t);
      debug !"Peer %{}: Timeout" t; 
      `Finished ()
    | `Result r -> result r 

  let start t = 
    debug !"Peer %{}: start message handler loop" t; 
    Deferred.repeat_until_finished () (wait_and_process_message t)
end

module Block_consumer = 
struct

  let process t b =
    let { Piece.b_index; Piece.off; Piece.len } = b in
    debug "Peer: consumer process block %d %d %d" b_index off len;
    M.Request (b_index, off, len) |> P.send t.peer;
    t.pending <- Set.add t.pending b

  let consume t () = 
    debug !"Peer %{}: trying to consume blocks (%d blocks)" t (Pipe.length t.block_rd); 
    match%bind Pipe.read t.block_rd with
    | `Eof -> `Finished () |> return
    | `Ok b -> 
      if (Set.length t.pending) < G.max_pending_request then (
        process t b; `Repeat () |> return
      ) else ( 
        Condition.wait t.few_pending >>| fun () -> `Repeat ()
      )

  let start t = 
    debug !"Peer %{}: start block consumer" t; 
    Deferred.repeat_until_finished () (consume t) |> don't_wait_for 

end 

module Block_producer =
struct 

  let produce_blocks' t nf i = 
    Nf.add_requested nf i;
    let l = Network_file.get_piece nf i |> Piece.blocks in
    debug !"Peer %{}: producing %d blocks for piece %d (pipe has currently %d blocks)" t (List.length l) i (Pipe.length t.block_wr); 
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
      debug !"Peer %{}: waiting can_produce" t;
      Condition.wait t.can_produce >>| fun () -> `Repeat ()
    | Some i -> 
      produce_blocks t nf i

  let start t nf = 
    debug !"Peer %{}: start block consumer" t; 
    Deferred.repeat_until_finished () (produce t nf) |> don't_wait_for

end

let set_nf t nf = 
  t.nf <- Some nf;
  if t.received_bitfield then
    push_event t Bitfield;
  Block_consumer.start t;
  Block_producer.start t nf

let close t = 
  debug !"Peer %{}: we close this peer." t;
  Pipe.close t.block_wr;
  Pipe.close t.event_wr;
  Peer_comm.close t.peer

let start t = 
  Message_loop.start t
  >>= fun () -> 
  close t

let send_bitfield t bf = 
  assert (not t.sent_bitfield);
  M.Bitfield bf |> P.send t.peer

(* TODO send keep alive when needed 
   let send_keep_alive t = P.send t.peer M.KeepAlive  *)

let send_have t i = M.Have i |> P.send t.peer 

let event_reader t = t.event_rd

let request_meta t = 
  debug !"Peer %{}: request meta" t;
  match t.meta with
  | None -> assert false
  | Some {id; total_length; num_block } -> 
    let f i = 
      let em = Extension.Request i in
      let m = M.Extended (id, Extension.to_bin em) in
      debug !"Peer %{}: sending ext. %{Extension}" t em;
      P.send t.peer m
    in
    List.range 0 num_block |> List.iter ~f

let status t = Status.{
    dl = t.conn_stat.total_dl / 1000;
    ul = t.conn_stat.total_ul / 1000;
    dl_speed = t.conn_stat.dl_speed /. 1000.;
    ul_speed = t.conn_stat.ul_speed /. 1000.;
    client = Peer_id.client (id t);
    addr = Peer_comm.addr t.peer;
  }

let validate_bitfield t = ()



