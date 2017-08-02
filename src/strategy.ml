
open Core
open Async
open Log.Global

module Ps = Peer_state

(** Find (piece_index, peer) to download from. 

    This is totally inefficient... we should define a datastructure that
    gets updated on the basis of new events, instead of recomputing this
    all the time *)

let next_requests file peers n : (int * Ps.t) list =

  let peers = Hashtbl.to_alist peers in
  let peers = List.map peers ~f:snd in

  let pieces_not_requested = File.pieces_not_requested file in

  (* returns first peer that have this piece and number of peers that have it
     TODO: we could choose a "better" peer instead of the first one
     we also returns the number of peers that have this list, so we can pick
     rarest pieces first *)
  let peers_having_piece i : (int * Ps.t * int) option = 
    let f p = 
      (Ps.has_piece p i) && not (Ps.is_peer_choking p) && not (Ps.is_idle p) 
    in
    let l = List.filter peers ~f in
    let n = List.length l in
    match l with
    | [] -> None 
    | h :: _ -> Some (i, h, n)   
  in
  let l1 = Bitset.to_list pieces_not_requested in 
  let l2 = List.filter_map l1 ~f:peers_having_piece in
  let cmp (_,_,c) (_,_,c') =  compare c c' in (* rarest first *)
  let l3 = List.permute l2 |> List.sort ~cmp in 
  List.take l3 n |> List.map ~f:(fun (a,b,_) -> (a,b)) 