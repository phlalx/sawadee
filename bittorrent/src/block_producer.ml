open Core
open Async
open Blog

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

let next_requests t sm = 

  let f i = 
    not (Shared_meta.is_downloaded sm i) && not (Shared_meta.is_requested sm i) && (has_piece t i)
  in

  List.range 0 (Shared_meta.num_pieces sm) |> List.filter ~f

let can_produce t sm =
  match next_requests t sm with
  | h :: _ when not t.peer_choking -> Some h
  | _ -> None

let produce_blocks' t sm i = 
  Shared_meta.add_requested sm i;
  let l = Shared_meta.get_piece sm i |> Piece.blocks in
  debug !"Peer_producer: producing %d new blocks for piece %d (pipe contains\
          %d blocks)" (List.length l) i (Pipe.length t.block_wr); 
  Deferred.List.iter l ~f:(Pipe.write_if_open t.block_wr)
  >>| fun () -> `Repeat ()

let produce_blocks t sm i =
  if Pipe.is_closed t.block_wr then
    `Finished () |> return
  else 
    produce_blocks' t sm i

let produce t sm () =
  match can_produce t sm with
  | None -> 
    debug !"Block_producer: block producer goes to sleep";
    (* TODO send not interested *)
    Condition.wait t.can_produce >>| fun () -> `Repeat ()
  | Some i -> 
    produce_blocks t sm i

let close t =
  Pipe.close t.block_wr
  
let start t sm = 
  info !"Block_producer: start block producer"; 
  Deferred.repeat_until_finished () (produce t sm) |> don't_wait_for

let create block_wr bitfield = {
  block_wr;
  bitfield;
  peer_choking = true;
  can_produce = Condition.create ();
}
