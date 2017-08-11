
open Core
open Async
open Log.Global

module P = Peer

(* we should define a proper datastructure to avoid recomputing this every 
  time *)
let next_requests l p n : (int * P.t) list =

  let peers_having_piece i : (int * P.t) option= 
    let f p = 
      (P.has_piece p i) &&   
      not (P.peer_choking p) && not (P.idle p) && (P.am_interested p)
    in 
    match List.filter p ~f |> List.permute with
    | [] -> None
    | p :: _ -> Some (i, p)
  in
  let l2 = List.filter_map l ~f:peers_having_piece in
  let l3 = List.permute l2 in 
  List.take l3 n