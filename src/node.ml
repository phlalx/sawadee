open Core
open Async
open Log.Global

type remote_node = {
  id : Node_id.t;
  status : [`Good| `Bad| `Questionable];
}

type t = {
  id : Node_id.t;
  routing : (Node_id.t * remote_node) list 
}

let create () = {
    id = Node_id.random ();
    routing = [];
}

