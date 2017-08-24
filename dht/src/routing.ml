open Core
open Async
open Dlog

type t = {
  mutable nis : Node_info.t list;
} 

let create () = {
  nis = [];
}

let add t (node_id, addr) = 
  match List.Assoc.find ~equal:(=) t.nis node_id with
  | None -> t.nis <- (node_id, addr) :: t.nis
  | Some _ -> () 

let k = 8

let trim_nodes_info info_hash nis = 
  let cmp (id1, _) (id2, _) = Node_id.compare info_hash id1 id2 in
  let l = List.sort nis ~cmp in 
  List.take l k

let trim_nodes_info' node_id nis =
  trim_nodes_info (Bt_hash.of_string (Node_id.to_string node_id)) nis

let k_closest t hash = trim_nodes_info hash t.nis

let find_node t id = 
  match trim_nodes_info' id t.nis with
  | ni :: _ when (fst ni) = id -> [ ni ]
  | l -> l 

let to_list t = t.nis