open Core
open Async
open Log.Global

type t = {
  routing : (Node_id.t * Node.t) list 
}

let create () = {
    routing = [];
}

