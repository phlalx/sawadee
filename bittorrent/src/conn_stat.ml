open Core
open Async
open Log.Global

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
