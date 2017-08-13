open Core open Async
open Log.Global

type t = {
  mutable routing : Node_info.t list 
}

let t = {
  routing = []
}

let equal x y = Node_id.to_string x = Node_id.to_string y
let find () = List.Assoc.find t.routing ~equal 

let try_add addr : unit Deferred.Or_error.t =
  let open Deferred.Or_error.Monad_infix in
  let n = Node.connect addr in
  Node.ping n 
  >>| fun id -> 
  if Option.is_none (find () id) then (
    t.routing <- (id, addr) :: t.routing;
    (* debug !"Krpc: %{Addr} = %{Node_id.to_string_hum} added" addr id *)
  ) 

let try_add_nis nis =
  let f (_, p) = try_add p |> Deferred.ignore in
  Deferred.List.iter ~how:`Parallel nis ~f
  >>| fun () ->
  info "Krpc: %d nodes added to table" (List.length t.routing)

let lookup_info_hash info_hash (_, addr) = 
  let n = Node.connect addr in 
  Node.get_peers n info_hash 
  >>| Result.ok

let k = 8

(* return the k first node_info closest to info_hash *)
let trim_nodes_info info_hash (nis : Node_info.t list) : Node_info.t list = 
  let cmp (id1, _) (id2, _) = Node_id.compare info_hash id1 id2 in
  let l = List.sort nis ~cmp in 
  List.take l k

let rec lookup_info_hash' (nis:Node_info.t list) info_hash ~depth : ((Node_info.t list) * (Addr.t list)) Deferred.t =

  if depth = 0 then
    (nis, []) |> return
  else 
    let%bind l = Deferred.List.filter_map nis ~f:(lookup_info_hash info_hash) ~how:`Parallel in 
    let f = function 
      | `Values x -> `Fst x 
      | `Nodes x -> `Snd x 
    in
    let values, nis = List.partition_map l ~f in
    let all_values = values |> List.concat in 
    let closest_nis = nis |> List.concat |> trim_nodes_info info_hash in 
    let%map better_nis, more_values = lookup_info_hash' closest_nis info_hash ~depth:(depth - 1) in
    better_nis, (more_values @ all_values)

let max_depth = 4 

let lookup info_hash = 
  let nis = trim_nodes_info info_hash t.routing in
  info "Krpc: querying %d closest peers" (List.length nis);
  match%map lookup_info_hash' nis info_hash ~depth:max_depth with  
  | nis, l -> 
    let f (n, _) = Node_id.distance_hash n info_hash in
    let dist = List.map nis ~f |> List.to_string ~f:string_of_int in
    info "Krpc: lookup. %d best distances %s" k dist;
    l 

(******************************)


let populate_from_hash info_hash = 

  let rec populate_aux (nis:Node_info.t list) info_hash ~depth acc : Node_info.t list Deferred.t =
    info "Krpc: populate %d" (List.length acc);
    match depth with
    | 0 -> return acc 
    | depth -> 
      begin
        let%bind l = Deferred.List.filter_map nis ~f:(lookup_info_hash info_hash) in 
        let f = function 
          | `Values x -> `Fst x 
          | `Nodes x -> `Snd x 
        in
        let _, nodes = List.partition_map l ~f in
        let new_nis = nodes |> List.concat in 
        populate_aux new_nis info_hash ~depth:(depth - 1) (new_nis @ acc)
      end    
  in
  let nis = List.take t.routing 3 in 
  populate_aux nis info_hash ~depth:2 []

let populate () = 
  if (List.length t.routing) <= 64 then 
    begin
      let%bind nis = Bt_hash.random () |> populate_from_hash in
      try_add_nis nis 
    end
  else
    Deferred.unit 

let init r = t.routing <- r

let table () = t.routing
