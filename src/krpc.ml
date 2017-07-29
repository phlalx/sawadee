open Core
open Async
open Log.Global

type t = {
  routing : (Node_id.t * Node.t) list 
}

let create () = {
    routing = [];
}

let try_add addr ~port =
  info "not yet implemented - try to add peer %s:%d"
    (Unix.Inet_addr.to_string addr) port