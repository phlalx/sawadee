open Core
open Async
open Blog

module Nf = Network_file

type t = {
  block_wr : Block.t Pipe.Writer.t;
  bitfield : Bitfield.t;
  mutable peer_choking : bool;
  can_produce : unit Condition.t;
}

let set_choking t b = 
  match b with
  | false -> 
    t.peer_choking <- false;
    debug "Block_producer: wake up block producer"; 
    Condition.signal t.can_produce ()
  | true -> 
    t.peer_choking <- true

let has_piece t i = Bitfield.get t.bitfield i

let next_requests t nf = 

  let f i = 
    not (Nf.is_downloaded nf i) && not (Nf.is_requested nf i) && (has_piece t i)
  in

  List.range 0 (Nf.num_pieces nf) |> List.filter ~f

let can_produce t nf =
  match next_requests t nf with
  | h :: _ when not t.peer_choking -> Some h
  | _ -> None

let produce_blocks' t nf i = 
  Nf.add_requested nf i;
  let l = Network_file.get_piece nf i |> Piece.blocks in
  debug !"Peer_producer: producing %d new blocks for piece %d (pipe contains\
          %d blocks)" (List.length l) i (Pipe.length t.block_wr); 
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
    debug !"Block_producer: block producer goes to sleep";
    (* TODO send not interested *)
    Condition.wait t.can_produce >>| fun () -> `Repeat ()
  | Some i -> 
    produce_blocks t nf i

let close t =
  Pipe.close t.block_wr
  
let start t nf = 
  info !"Block_producer: start block producer"; 
  Deferred.repeat_until_finished () (produce t nf) |> don't_wait_for

let create block_wr bitfield = {
  block_wr;
  bitfield;
  peer_choking = true;
  can_produce = Condition.create ();
}
