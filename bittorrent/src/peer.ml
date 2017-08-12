open Core
open Async
open Log.Global

module Nf = Network_file

module Conn_stat = 
struct

  let tick = sec 1.0 
  let window_size = 20

  type t = {
    mutable total_dl : int;
    mutable total_ul : int;
    last_n_sec : (int * int) Queue.t;
    mutable dl_speed : float;
    mutable ul_speed : float;
    mutable last_tick_dl : int;
    mutable last_tick_ul : int;
  } [@@deriving sexp]

  let to_string t = Sexp.to_string (sexp_of_t t)

  let incr_dl t i = t.last_tick_dl <- t.last_tick_dl + i

  let incr_ul t i = t.last_tick_ul <- t.last_tick_ul + i

  let sum q = Queue.fold q ~f:(fun (x,y) (a,b) -> (x+a, y+b)) ~init:(0,0) 

  let update t () = 
    t.total_dl <- t.total_dl + t.last_tick_dl;
    t.total_ul <- t.total_ul + t.last_tick_ul;
    Queue.enqueue t.last_n_sec (t.last_tick_dl, t.last_tick_ul);
    if (Queue.length t.last_n_sec > window_size) then
      Queue.dequeue t.last_n_sec |> ignore;

    let (last_dl, last_ul) = sum t.last_n_sec in

    t.dl_speed <- (float_of_int last_dl) /. (float_of_int window_size);
    t.ul_speed <- (float_of_int last_ul) /. (float_of_int window_size)


  (* tick could  be done outside this module for all peers at once *)
  let start t = Clock.every tick (update t)

  let create () = 
    let t = {
      total_dl = 0;
      total_ul = 0;
      dl_speed = 0.;
      ul_speed = 0.;
      last_n_sec = Queue.create ();
      last_tick_dl = 0;
      last_tick_ul = 0;
    }
    in start t; t

end

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
  | Bitfield of Bitfield.t sexp_opaque
  | Piece of int
  | Fail of int
  | More
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
  peer : Peer_comm.t;
  extension : bool;
  dht : bool;
  mutable peer_choking : bool; 
  mutable peer_interested : bool;
  mutable am_choking : bool;
  mutable am_interested : bool; 
  mutable idle : bool;
  mutable port : int option;
  bitfield : Bitfield.t;
  wr : event Pipe.Writer.t; 
  rd : event Pipe.Reader.t;
  mutable meta : meta option; (* protocol id and metadata size *)
  mutable nf : Network_file.t option;
  mutable sent_bitfield : bool;
  conn_stat : Conn_stat.t;
  block_wr : block Pipe.Writer.t;
  block_rd : block Pipe.Reader.t;
  mutable pending : block Set.Poly.t;
  few_pending : unit Condition.t;
  received_bitfield : bool
} [@@deriving fields]

let create peer ~dht ~extension =
  let rd, wr = Pipe.create () in 
  let block_rd, block_wr = Pipe.create () in
  {
    peer;
    peer_interested = false; 
    peer_choking = true; 
    am_interested = false;
    am_choking = true;
    idle = false;
    port = None;
    bitfield = Bitfield.empty G.max_num_pieces;
    rd;
    wr;
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
    received_bitfield = false;
  }

let event_to_string e = Sexp.to_string (sexp_of_event e)

exception Incorrect_behavior

let validate t b = if not b then raise Incorrect_behavior

let id t = Peer_comm.id t.peer

let to_string t = id t |> Peer_id.to_string_hum  

(* TODO add validation check *)
let has_piece t i = Bitfield.get t.bitfield i

let bitfield t = t.bitfield

let is_or_not b = if b then "" else "not"

let set_peer_interested t b = 
  debug !"Peer %{}: is %{is_or_not} interested" t b;
  t.peer_interested <- b

let set_peer_choking t b = 
  debug !"Peer %{}: is %{is_or_not} choking" t b;
  t.peer_choking <- b

let set_am_interested t b = 
  debug !"Peer %{}: I am %{is_or_not} interested" t b;
  t.am_interested <- b;
  (if b then M.Interested else M.Not_interested) |> P.send t.peer

let set_am_choking t b = 
  debug !"Peer %{}: I am %{is_or_not} choking" t b;
  t.am_choking <- b;
  (if b then M.Choke else M.Unchoke) |> P.send t.peer

let request_piece t i =
  let nf = Option.value_exn t.nf in
  debug !"Peer %{}: we request %d (pipe has currently %d blocks) " t i (Pipe.length t.wr); 
  Network_file.get_piece nf i |> Piece.blocks
  |> List.iter ~f:(Pipe.write_without_pushback_if_open t.block_wr)

let requesting_loop t () =

  let process_block b =
    let { Piece.b_index; Piece.off; Piece.len } = b in
    debug "Peer: process block %d %d %d" b_index off len;
    M.Request (b_index, off, len) |> P.send t.peer;
    t.pending <- Set.add t.pending b
  in 

  Condition.wait t.few_pending 
  >>= fun () ->
  let slot_available = G.max_pending_request - (Set.length t.pending) in
  info !"Peer: %{} request queue has %d slots available" t slot_available;
  match%map Pipe.read' t.block_rd ~max_queue_length:slot_available with
  | `Eof -> 
    `Finished ()
  | `Ok q -> 
    info !"Peer: %{} managed to fill %d slots" t (Queue.length q);
    Queue.iter q ~f:process_block;
    `Repeat ()

let process_block t nf index bgn block =
  let piece = Network_file.get_piece nf index in
  let len = String.length block in
  Network_file.is_valid_piece_index nf index |> validate t; 
  Piece.is_valid_block piece bgn len |> validate t;
  let b = Piece.{ b_index = index; off = bgn; len} in
  assert (Set.mem t.pending b);
  t.pending <- Set.remove t.pending b;
  if (Set.length t.pending) < G.max_pending_request then ( 
    debug !"Peer %{}: signal pending" t;
    Condition.signal t.few_pending ()
  );
  match Piece.update piece bgn block with 
  | `Ok -> 
    info !"Peer %{}: got block - piece %d offset = %d" t index bgn
  | `Hash_error -> 
    debug !"Peer %{}: hash error piece %d" t index
  (* TODO define some event *)
  | `Downloaded ->
    info !"Peer %{}: got piece %d" t index; 
    P.set_downloading t.peer;
    Pipe.write_without_pushback t.wr (Piece index)

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
    Tinfo tinfo |> Pipe.write_without_pushback t.wr  
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
    Pipe.write_without_pushback t.wr Support_meta
  | Extension.Data (i, s) -> update_data t i s  
  | _ -> info !"Peer %{}: not implemented" t

let process_request t nf index bgn length =
  let piece = Network_file.get_piece nf index in
  Network_file.is_valid_piece_index nf index |> validate t;
  Piece.is_valid_block_request piece bgn length |> validate t;
  Network_file.has_piece nf index |> validate t;
  P.set_uploading t.peer; (* TODO pass this in this module *)
  let piece = Network_file.get_piece nf index in
  (* TODO: we could avoid a string allocation by using a substring 
     for the block in M.Piece *)

  let block = Piece.get_content piece ~off:bgn ~len:length in
  Conn_stat.incr_ul t.conn_stat length;
  Message.Block (index, bgn, block) |> P.send t.peer

let process_message t m : unit =
  debug !"Peer %{}: received %{Message}" t m;
  match m with

  | M.KeepAlive -> ()

  | M.Choke -> 
    set_peer_choking t true;
    Pipe.write_without_pushback t.wr Choke

  | M.Unchoke -> 
    set_peer_choking t false; 
    Pipe.write_without_pushback t.wr Unchoke

  | M.Interested -> 
    set_peer_interested t true; 
    Pipe.write_without_pushback t.wr Interested

  | M.Not_interested -> 
    set_peer_interested t false;
    Pipe.write_without_pushback t.wr Not_interested

  | M.Bitfield bits -> 
    info !"Peer %{}: received bitfield (%d pieces)" t (Bitfield.card bits);
    Bitfield.copy ~src:bits ~dst:t.bitfield; 
    validate t (not t.received_bitfield);
    if Option.is_some t.nf then
      Pipe.write_without_pushback t.wr (Bitfield bits)

  | M.Port port -> 
    debug !"Peer %{}: received port %d" t port;
    set_port t (Some port);
    if G.is_dht () then (
      Addr.create (Peer_comm.addr t.peer) port |> 
      Krpc.try_add |> Deferred.ignore |> don't_wait_for )

  | M.Extended (id, b) -> 
    if Option.is_none t.nf then 
      process_extended t id b 

  | M.Have index -> 
    debug !"Peer %{}: received have %d" t index;
    Bitfield.set t.bitfield index true;
    if Option.is_some t.nf then
      Pipe.write_without_pushback t.wr (Have index)

  (* the following messages must have nf set *)

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
    failwith "not implemented yet"

let clear_requests t =
  let f { Piece.b_index } =
    debug !"Peer %{}: clear requests %d" t b_index;
    Fail b_index |> Pipe.write_without_pushback t.wr 
  in
  Set.iter t.pending  ~f

(* This is the main message processing loop. We consider two types of events.
   Timeout (idle peer), and message reception. *)
let rec wait_and_process_message t () =

  let result = function
    | `Ok m -> process_message t m; `Repeat ()
    | `Eof -> 
      clear_requests t;
      `Finished ()
  in
  P.receive t.peer |> Clock.with_timeout G.keep_alive
  >>| function 
  | `Timeout -> 
    clear_requests t;
    `Finished ()
  | `Result r -> result r 

let set_nf t nf = 
  t.nf <- Some nf;
  if t.received_bitfield then
    Bitfield t.bitfield |> Pipe.write_without_pushback t.wr 

let start t =
  debug !"Peer %{}: start requesting loop" t; 
  Deferred.repeat_until_finished () (requesting_loop t) |> don't_wait_for;
  Condition.signal t.few_pending ();
  debug !"Peer %{}: start message handler loop" t; 
  Deferred.repeat_until_finished () (wait_and_process_message t)
  >>| fun () -> 
  debug !"Peer %{}: quit message handler loop" t; 
  Pipe.close t.wr;
  Pipe.close t.block_wr

let close t = 
  debug !"Peer %{}: we close this peer." t;
  Peer_comm.close t.peer

let send_bitfield t bf = 
  assert (not t.sent_bitfield);
  M.Bitfield bf |> P.send t.peer

let send_keep_alive t = 
  P.send t.peer M.KeepAlive 

let advertise_piece t i = M.Have i |> P.send t.peer 

let event_reader t = t.rd

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
    List.range 0 num_block 
    |> List.iter ~f

let status t = Status.{
    dl = t.conn_stat.total_dl;
    ul = t.conn_stat.total_ul;
    dl_speed = t.conn_stat.dl_speed;
    ul_speed = t.conn_stat.ul_speed;
    client = Peer_id.client (id t);
    addr = Peer_comm.addr t.peer;
  }

let is_interesting t = 
  let nf = Option.value_exn t.nf in
  let downloaded = Nf.downloaded nf in 
  let num_pieces = Nf.num_pieces nf in
  let bf = t.bitfield in
  (*   let s = Bitfield.to_list bf num_pieces |> List.to_string ~f:string_of_int in info "bitfield %s"  s; *)  
  not (Bitfield.is_subset num_pieces bf downloaded)

let take_request t = (Pipe.length t.block_wr) <= 2 * G.max_pending_request

let validate_bitfield t = ()



