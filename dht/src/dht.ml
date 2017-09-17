open Core
open Async
open Dlog

type t = {
  id : Node_id.t;
  routing : Routing.t;
  peers : Peers_tbl.t;
  tokens : Tokens.t;
  token_time : Time.Span.t
}

let to_string t = Node_id.to_string_hum t.id

let start_server port id routing peers tokens = 
  (let%map ns = Node_server.create port id routing peers tokens in
   Node_server.start ns) |> don't_wait_for

let set_verbose i =
    match i with
    | 0 -> set_level `Error
    | 1 -> set_level `Info 
    | 2 -> set_level `Debug
    | _ -> failwith "verbose level should be 1 or 2"

let create ?token_time ~port id ~data_path ~verbose = 
  set_verbose verbose;
  let f = Log.Output.file `Text (data_path ^ "/dht_log") in 
  set_output [f; (Log.Output.stderr ())];

  let routing = Routing.create () in
  let peers = Peers_tbl.create () in
  let tokens = Tokens.create () in
  start_server port id routing peers tokens;
  let t =  {
    id;
    routing;
    peers;
    tokens;
    token_time = Option.value ~default:(sec 20.0) token_time;
  } in 
  info !"Dht: %{} created port %d" t port;
  t

let try_add t addr : unit Deferred.Or_error.t =
  let open Deferred.Or_error.Let_syntax in
  let%map id = Node.ping t.id addr in
  info !"Dht: %{} added node %{Node_id.to_string_hum}" t id;
  Routing.add t.routing (id, addr)

let lookup_hash t hash (_, addr) = 
  Node.get_peers t.id addr hash >>| Result.ok


(* return the k first node_info closest to hash *)
let trim_nodes_info k hash (nis : Node_info.t list) : Node_info.t list = 
  let cmp (id1, _) (id2, _) = Node_id.compare hash id1 id2 in
  let l = List.sort nis ~cmp in 
  List.take l k

let k = 8

(* query in parallel k nodes closest to infohash
   keep the k best answers and accumulate values found 
   repeat until at least one answer 

   this isn't exactly what should be done. In particular, we shoudln't do more
   than alpha = 3 simultaneous requests. TODO see xlattice doc *)

let rec lookup_hash' t (nis:Node_info.t list) hash ~depth 
  : ((Node_info.t list) * (Addr.t list)) Deferred.t =
  let f (n, _) = Node_id.distance_hash n hash in
  let dist_list = List.map nis ~f in
  let dist_list_str = List.to_string Int.to_string dist_list in
  info "Dht: lookup_hash' depth = %d closest nodes %s" depth dist_list_str;
  if nis = [] then (
    assert (depth >= 0);
    (nis, []) |> return
  ) else 
    let%bind l = 
      Deferred.List.filter_map nis ~f:(lookup_hash t hash) 
        ~how:`Parallel in 
    let f = function 
      | `Values x -> `Fst x 
      | `Nodes x -> `Snd x 
    in
    let values, nis = List.partition_map l ~f in
    let all_values = values |> List.concat in 
    let closest_nis = nis |> List.concat |> trim_nodes_info k hash in 
    let%map better_nis, more_values = lookup_hash' t closest_nis hash 
        ~depth:(depth - 1) in
    better_nis, (List.dedup_and_sort ~compare (more_values @ all_values))

let max_depth = 100 

let lookup t hash = 
  let nis = Routing.k_closest t.routing hash in
  match%map lookup_hash' t nis hash ~depth:max_depth with  
  | nis, l -> 
    info !"Dht: %{} lookup %{Bt_hash.to_string_hum} - found %d peers" t hash
     (List.length l);
    l

let table t = Routing.to_list t.routing

let announce t hash ~port = 
  (* TODO add us to the list of known peers *)
  let addr = Addr.local ~port in
  info !"Dht: %{} announces %{Addr} %{Bt_hash.to_string_hum}" t addr hash;
  Peers_tbl.add t.peers hash addr;
  let f (token, addr) = 
    Node.announce t.id addr hash port token |> Deferred.ignore |> don't_wait_for 
  in
  Tokens.iter t.tokens ~f






