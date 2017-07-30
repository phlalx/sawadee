open Core open Async
open Log.Global
module BU = Bencode_utils
module G = Global
module Em = Error_msg

type t = {
  mutable routing : (Node_id.t * Node.t) list 
}

let t = {
  routing = []
}

(* TODO make this type globally available *)
type node_info = Node_id.t * Socket.Address.Inet.t 

let table_to_node_info () : node_info list = 
  let f (i, n) = (i, Node.addr n) in List.map t.routing ~f

let equal x y = Node_id.to_string x = Node_id.to_string y
let find () = List.Assoc.find t.routing ~equal 

let try_add addr : unit Deferred.Or_error.t =
  debug "try reaching node %s" (Socket.Address.Inet.to_string addr);
  let open Deferred.Or_error.Monad_infix in
  let n = Node.create addr in
  Node.ping n 
  >>| fun id -> 
  if Option.is_none (find () id) then (
    (* TODO do this test before the ping? *)
    (* TODO enforce invariant if some(id), status != questionable *)
    Node.set_id n id; 
    Node.set_status n `Good;
    t.routing <- (id, n) :: t.routing;
    info "added node nodeid(%s) = %s" (Socket.Address.Inet.to_string addr) 
      (Node.to_string n)
  ) 


let lookup_info_hash info_hash (_, addr) = 
  let n = Node.create addr in 
  Node.get_peers n info_hash 
  >>| Result.ok

(* TODO we return the k first node_info closest to info_hash *)
let trim_nodes_info info_hash (nis : node_info list) : node_info list = nis

let rec lookup_info_hash' (nis:node_info list) info_hash ~depth : 
  Socket.Address.Inet.t list Deferred.Option.t =
  if depth = 0 then
    return None 
  else (
    let%bind l = Deferred.List.filter_map nis ~f:(lookup_info_hash info_hash) in 
    let f = function 
      | `Values x -> `Fst x 
      | `Nodes x -> `Snd x 
    in
    let values, nodes = List.partition_map l ~f in
    let combined_values = values |> List.concat in 
    let combined_nis = nodes |> List.concat in 
    match combined_values with 
    | [] -> lookup_info_hash' combined_nis info_hash ~depth:(depth - 1)
    |  _  -> return (Some combined_values))

let max_depth = 4 

let lookup info_hash = 
  let nis = table_to_node_info () |> trim_nodes_info info_hash in
  match%bind lookup_info_hash' nis info_hash ~depth:max_depth with  
  | Some x -> return x 
  | None -> return []

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


let read_routing_table () = 
  let routing_table_name = sprintf "%s/%s" (G.path ()) G.routing_table_name in 
  let routing_table = 
    try
      In_channel.read_all routing_table_name
    with _ -> 
      info "can't read routing table %s. Using empty table" routing_table_name;
      ""
  in
  let decoded_table = 
    try
      table_of_string routing_table 
    with _ -> 
      info "can't decode routing table %s. Using empty table" routing_table_name;
      [] 
  in

  let f (_, p) = try_add p |> Deferred.ignore in
  Deferred.List.iter decoded_table ~f

let write_routing_table () =
  let routing_table_name = sprintf "%s/%s" (G.path ()) G.routing_table_name in 
  try 
    let table  = let f (i, n) = (i, Node.addr n) in List.map t.routing ~f in
    Out_channel.write_all routing_table_name ~data:(table_to_string table);
    info "writing routing table to file %s" routing_table_name;
  with
  (* TODO print error in debug *)
    _ -> Print.printf "%s\n" (Em.can't_open routing_table_name)