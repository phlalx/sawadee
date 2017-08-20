open Core
open Async
open Log.Global

type t = {
  id : Node_id.t;
  mutable routing : Node_info.t list 
}

let create ~port id = 

  
  info !"Dht: peer-id:%{Node_id.to_string_hum}" id;
  {
    id;
    routing = [];
  }

let try_add t addr : unit Deferred.Or_error.t =
  let open Deferred.Or_error.Let_syntax in
  let n = Node.connect t.id addr in
  let%map id = Node.ping n in
  match List.Assoc.find ~equal:(=) t.routing id with
  | None -> 
    (* debug !"Dht: %{Addr} = %{Node_id.to_string_hum} added" addr id; *)
    t.routing <- (id, addr) :: t.routing 
  | Some _ -> ()

let lookup_info_hash t info_hash (_, addr) = 
  let n = Node.connect t.id addr in 
  Node.get_peers n info_hash >>| Result.ok

let k = 8

(* return the k first node_info closest to info_hash *)
let trim_nodes_info t info_hash (nis : Node_info.t list) : Node_info.t list = 
  let cmp (id1, _) (id2, _) = Node_id.compare info_hash id1 id2 in
  let l = List.sort nis ~cmp in 
  List.take l k

let rec lookup_info_hash' t (nis:Node_info.t list) info_hash ~depth 
  : ((Node_info.t list) * (Addr.t list)) Deferred.t =
  if depth = 0 then
    (nis, []) |> return
  else 
    let%bind l = 
      Deferred.List.filter_map nis ~f:(lookup_info_hash t info_hash) 
        ~how:`Parallel in 
    let f = function 
      | `Values x -> `Fst x 
      | `Nodes x -> `Snd x 
    in
    let values, nis = List.partition_map l ~f in
    let all_values = values |> List.concat in 
    let closest_nis = nis |> List.concat |> trim_nodes_info t info_hash in 
    let%map better_nis, more_values = lookup_info_hash' t closest_nis info_hash 
        ~depth:(depth - 1) in
    better_nis, (more_values @ all_values)

let max_depth = 4 

let lookup t ?populate info_hash = 
  (* TODO populate the table if needed *)
  let nis = trim_nodes_info t info_hash t.routing in
  info "Dht: querying %d closest peers" (List.length nis);
  match%map lookup_info_hash' t nis info_hash ~depth:max_depth with  
  | nis, l -> 
    let f (n, _) = Node_id.distance_hash n info_hash in
    let dist = List.map nis ~f |> List.to_string ~f:string_of_int in
    info "Dht: lookup. %d best distances %s" k dist;
    l 

let init t r = t.routing <- r

let table t = t.routing
