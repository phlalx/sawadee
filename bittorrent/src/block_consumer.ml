open Core
open Async
open Blog

module Pc = Peer_comm
module G = Global

type t = {
  can_consume : unit Condition.t; 
  block_rd : Block.t Pipe.Reader.t;
  mutable pending : Block.t Set.Poly.t;
  peer : Peer_comm.t;
}

let process t b =
  debug !"Block_consumer: requesting block %{Block}" b;
  Pc.send t.peer (Message.Request b);
  t.pending <- Set.add t.pending b

let consume t () = 
  (* debug !"Block_consumer: trying to consume a block (%d blocks in pipe)" (Pipe.length t.block_rd);  *)
  if (Set.length t.pending) < G.max_pending_request then (
    match%bind Pipe.read t.block_rd with
    | `Eof -> `Finished () |> return
    | `Ok b -> process t b; `Repeat () |> return
  ) else (
    (* debug !"Block consumer goes to sleep";  *)
    Condition.wait t.can_consume >>| fun () -> `Repeat ()
  )

let pending_requests t =
  Set.Poly.map t.pending ~f:(fun { Block.piece } -> piece ) 
  |> Set.to_list 

let notify t b = 
  (*   Set.mem t.pending b |> validate t; TODO add validation *)
  t.pending <- Set.remove t.pending b;
  if (Set.length t.pending) < G.max_pending_request then ( 
    (* info !"Block_consumer: wake-up block consumer"; *)
    Condition.signal t.can_consume ()
  )

let start t = 
  (* info !"Block_consumer: start";  *)
  Deferred.repeat_until_finished () (consume t) |> don't_wait_for 

let create block_rd peer = {
  can_consume = Condition.create ();
  block_rd;
  pending = Set.Poly.empty;
  peer;
}