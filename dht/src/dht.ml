open Core
open Async
open Log.Global

type t = {
  id : Node_id.t;
  routing : Routing.t;
  peers : Peers_tbl.t;
  tokens : Tokens.t;
}

let start_server port id routing peers tokens = 
  (let%map ns = Node_server.create port id routing peers tokens in
   Node_server.start ns) |> don't_wait_for

let create ~port id = 
  info !"Dht: peer-id:%{Node_id.to_string_hum}" id;
  let routing = Routing.create () in
  let peers = Peers_tbl.create () in
  let tokens = Tokens.create () in {
    id;
    routing;
    peers;
    tokens;
  }

let try_add t addr : unit Deferred.Or_error.t =
  let open Deferred.Or_error.Let_syntax in
  let n = Node.connect t.id addr in
  let%map id = Node.ping n in
  Routing.add t.routing (id, addr)

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
  let nis = Routing.k_closest t.routing info_hash in
  info "Dht: querying %d closest peers" (List.length nis);
  match%map lookup_info_hash' t nis info_hash ~depth:max_depth with  
  | nis, l -> 
    let f (n, _) = Node_id.distance_hash n info_hash in
    let dist = List.map nis ~f |> List.to_string ~f:string_of_int in
    info "Dht: lookup. %d best distances %s" k dist;
    l 

let table t = Routing.to_list t.routing

let announce t hash ~port = 
  let f (token, addr) = 
    let n = Node.connect t.id addr in
    Node.announce n hash port token |> Deferred.ignore |> don't_wait_for 
  in
  Tokens.iter t.tokens ~f






