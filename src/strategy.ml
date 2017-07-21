
open Core
open Async
open Log.Global

module P = Peer

(** Find a piece and a peer to download from. *)
let next_request file peers : (Piece.t * P.t) Option.t =

  let pieces_not_requested = File.pieces_not_requested file in

  let f peer = 
    if (P.is_idle peer) || (P.is_peer_choking peer) then
      None
    else 
      let pieces_owned_by_peer = Peer.owned_pieces peer in
      let pieces_to_request = Bitset.inter pieces_not_requested pieces_owned_by_peer in
      match Bitset.choose_random pieces_to_request with 
      | None -> None 
      | Some i -> Some (File.get_piece file i, peer)
  in
    let l = List.map peers ~f in
    match List.find l ~f:is_some with
    | None -> None
    | Some x -> x