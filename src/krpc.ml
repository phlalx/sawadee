open Core
open Async
open Log.Global
module BU = Bencode_utils

type t = {
  mutable routing : (Node_id.t * Node.t) list 
}

let t = {
  routing = []
}

let try_add addr ~port =
  info "not yet implemented - try to add peer %s:%d"
    (Unix.Inet_addr.to_string addr) port

let table_to_string table =
  let f (id, p) =
    let sid = Node_id.to_string id in
    let sn = BU.peer_to_string p in
    sid ^ sn
  in
  List.map table ~f
  |> String.concat

(* TODO: create a module for compact representation of peer addresses *)
let compact_length = 6

let table_of_string s = 
  let f s = 
      let s1 = String.sub s 0 Node_id.length in
      let s2 = String.sub s Node_id.length compact_length in
      (Node_id.of_string s1), (BU.string_to_peer s2)
  in
  Bencode_utils.split_list s (Node_id.length + compact_length) 
  |> List.map ~f

let table () = 
  let f (i, n) = (i, Node.addr n) in List.map t.routing ~f







    