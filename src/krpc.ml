open Core
open Async
open Log.Global
module BU = Bencode_utils
module G = Global

type t = {
  mutable routing : (Node_id.t * Node.t) list 
}

let t = {
  routing = []
}

let equal x y = Node_id.to_string x = Node_id.to_string y
let find = List.Assoc.find t.routing ~equal 

open Krpc_packet

let try_add addr =
  let open Deferred.Or_error.Monad_infix in
  info "########## trying to add peer %s" (Socket.Address.Inet.to_string addr);
  let n = Node.create addr in
  Node.ping n 
  >>| fun id -> 
  if Option.is_none (find id) then (
    (* TODO do this test before the ping? *)
    (* TODO enforce invariant if some(id), status != questionable *)
    Node.set_id n id; 
    Node.set_status n `Good;
    info "########## it worked";
    info "but already in the table";
    t.routing <- (id, n) :: t.routing  
  ) 

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







