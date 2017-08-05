open Core
open Async
open Log.Global

module M = Message
module P = Peer_comm
module G = Global

type event = 
  | Choke 
  | Unchoke
  | Interested
  | Not_interested 
  | Have
  | Bitfield
  | Bye

type t = {
  peer : Peer_comm.t;
  extension : bool;
  dht : bool;
  mutable peer_choking : bool; 
  mutable peer_interested : bool;
  mutable am_choking : bool;
  mutable am_interested : bool; 
  mutable idle : bool;
  mutable port : int Option.t;
  bitfield : Bitfield.t;
  wr : event Pipe.Writer.t; 
  rd : event Pipe.Reader.t;
  requested : (int, unit Ivar.t) Hashtbl.t;
  support_metadata : int Option.t Ivar.t;
  metadata_size : int Option.t Ivar.t;
  mutable nf : Network_file.t Option.t
} [@@deriving fields]

let create peer ~dht ~extension =
  let rd, wr = Pipe.create () in {
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
    requested = Hashtbl.Poly.create ();
    extension;
    dht;
    support_metadata = Ivar.create ();
    metadata_size = Ivar.create (); 
    nf = None
  }

let request_metadata t ~len = assert false

let support_metadata t = 
  if t.extension then
    Ivar.read t.support_metadata
  else 
    return None

let request_metadata_size t = 
  if t.extension then
    Ivar.read t.metadata_size 
  else 
    return None

let event_to_string = function
  | Choke -> "Choke"
  | Unchoke -> "Unchoke"
  | Interested -> "Interested"
  | Not_interested -> "Not_interested"
  | Bitfield -> "Bitfield"
  | Have -> "Have"
  | Bye -> "Bye"

exception Incorrect_behavior

let validate t b = if not b then raise Incorrect_behavior

let id t = Peer_comm.id t.peer

let to_string t = id t |> Peer_id.to_string_hum  

(* TODO add validation check *)
let has_piece t i = Bitfield.get t.bitfield i

let bitfield t = t.bitfield

let is_or_not b = if b then "" else "not"

let set_peer_interested t b = 
  info !"Peer %{}: is %{is_or_not} interested" t b;
  t.peer_interested <- b

let set_peer_choking t b = 
  info !"Peer %{}: is %{is_or_not} choking" t b;
  t.peer_choking <- b

let set_am_interested t b = 
  info !"Peer %{}: I am %{is_or_not} interested" t b;
  t.am_interested <- b;
  (if b then M.Interested else M.Not_interested) |> P.send t.peer

let set_am_choking t b = 
  info !"Peer %{}: I am %{is_or_not} choking" t b;
  t.am_choking <- b;
  (if b then M.Choke else M.Unchoke) |> P.send t.peer

(* we always request all blocks from a piece to the same peer at the 
   same time *)
let request_piece t i =
  let nf = Option.value_exn t.nf in
  let open Network_file in

  let on_ivar = function 
    | `Timeout -> 
      info !"Peer %{}: cancelling request" t;
      (* TODO better to do this in pwp? *)
      (* don't forget to put peer to idle *)
      Network_file.set_piece_status nf i `Not_requested;
      set_idle t true
    | `Result () -> 
      (* TODO better to do this in pwp? *)
      Network_file.set_piece_status nf i `Downloaded;
      Network_file.write_piece nf i
      (* TODO change interface of network file *)
  in 

  info !"Peer %{}: we request %d" t i; 
  (* TODO assert not requested *)
  Network_file.set_piece_status nf i `Requested; 
  let f ~index ~off ~len =
    M.Request(index, off, len) |> P.send t.peer 
  in 
  Network_file.get_piece nf i |> Piece.iter ~f;
  let ivar = Ivar.create () in
  Hashtbl.add_exn t.requested ~key:i ~data:ivar;
  (* TODO error case, if hash is wrong *)
  Ivar.read ivar |> Clock.with_timeout G.idle  
  >>| 
  on_ivar 
  >>| fun () ->
  Hashtbl.remove t.requested i

let process_block t nf index bgn block =
  match Hashtbl.find t.requested index with 
  | None -> () (* probably means we already canceled this request *)
  | Some ivar ->
    let open Network_file in
    let piece = Network_file.get_piece nf index in
    let len = String.length block in
    Network_file.is_valid_piece_index nf index |> validate t; 
    Piece.is_valid_block piece bgn len |> validate t;
    match Piece.update piece bgn block with 
    | `Ok -> 
      debug !"Peer %{}: got block - piece %d offset = %d" t index bgn
    | `Hash_error -> 
      info !"Peer %{}: hash error piece %d" t index
    (* TODO write something else on ivar. Now it will just time out *)
    | `Downloaded ->
      info !"Peer %{}: got piece %d" t index; 
      P.set_downloading t.peer;
      Ivar.fill ivar ()

(*  let request_meta len = 
    let open Extension in
    t.info.state <- `Requested;
    let f ~index ~off ~len =
    Request index |> send_extended st `Metadata_ext 
    in
    let p = Piece.create ~index:0 (Bt_hash.random ()) ~len in 
    Piece.iter ~f p
*)

let process_handshake t id s =
  let em = Extension.of_bin `Handshake s in 
  info !"Peer %{}: extended message %d %{Extension}" t id em; 
  match em with 
  | Extension.Handshake [`Metadata (id, s)] ->
    Some id |> Ivar.fill t.support_metadata;
    Some s |> Ivar.fill t.metadata_size;
  | _ -> 
    None |> Ivar.fill t.support_metadata;
    None |> Ivar.fill t.metadata_size

let process_metadata t id s =
  match Ivar.peek t.support_metadata with
  | None -> validate t false 
  | Some (Some i) when i = id -> 
    let em = Extension.of_bin `Metadata_ext s in 
    info !"Peer %{}: extended message %d %{Extension}" t id em
  | _ -> ()

let process_extended t id s =
  if id = 0 then 
    process_handshake t id s  
  else
    process_metadata t id s 

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
  (* TODO info !"Peer %{}: has %d pieces" t (Bitset.card t.have)  *)
  (* TODO validate this bitfield *)
    info !"Peer %{}: received bitfield" t;
    Bitfield.copy ~src:bits ~dst:t.bitfield; 
    Pipe.write_without_pushback t.wr Bitfield

  | M.Port port -> 
    info !"Peer %{}: received port %d" t port;
    set_port t (Some port);
    if G.is_node () then (
      Addr.create (Peer_comm.addr t.peer) port |> 
      Krpc.try_add |> Deferred.ignore |> don't_wait_for )

  | M.Extended (id, b) -> 
    process_extended t id b 

  | M.Have index -> 
    info !"Peer %{}: received have %d" t index;
    Bitfield.set t.bitfield index true;
    Pipe.write_without_pushback t.wr Have 

  (* the following messages must have nf set *)

  | M.Request (index, bgn, length) -> 
    info !"Peer %{}: peer requests %d" t index;
    let nf = assert false in
    if not (am_choking t) then 
      process_request t nf index bgn length

  | M.Block (index, bgn, block) -> 
    let nf = assert false in
    process_block t nf index bgn block

  | M.Cancel (index, bgn, length) -> 
    failwith "not implemented yet"


let leaving t = 
  let f x = Ivar.fill x () in
  Hashtbl.iter t.requested ~f; 
  (* TODO: fill ivars *)
  Pipe.close t.wr; never ()

(* This is the main message processing loop. We consider two types of events.
   Timeout (idle peer), and message reception. *)
let rec wait_and_process_message t : unit Deferred.t =

  let result = function
    | `Ok m -> process_message t m |> return
    | `Eof -> leaving t  
  in
  P.receive t.peer |> Clock.with_timeout G.idle 
  >>= function 
  | `Timeout -> leaving t
  | `Result r -> result r 

let set_nf t nf = t.nf <- Some nf

let start t : unit =
  info !"Peer %{}: start message handler loop" t; 
  Deferred.forever () (fun () -> wait_and_process_message t)

let send_bitfield t bf = M.Bitfield bf |> P.send t.peer

let advertise_piece t i = M.Have i |> P.send t.peer 

let read_event t = 
  match%map Pipe.read t.rd with
  | `Eof -> Bye
  | `Ok e ->  e









