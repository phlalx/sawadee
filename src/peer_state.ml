open Core
open Async
open Log.Global

type t = {
  peer : Peer.t;
  mutable peer_choking : bool; 
  mutable peer_interested : bool;
  mutable am_choking : bool;
  mutable am_interested : bool; 
  mutable have : Bitset.t;
  mutable pending : Int.Set.t;
  mutable idle : bool;
  mutable port : int Option.t;
  mutable bitfield : Bitfield.t Option.t;
  mutable metadata_id : int Option.t;
  mutable metadata_size : int Option.t
} [@@deriving fields]


let create peer = {
    peer;
    peer_interested = false; 
    peer_choking = true; 
    am_interested = false;
    am_choking = true;
    have = Bitset.empty 0; 
    pending = Int.Set.empty;
    idle = false;
    port = None;
    bitfield = None;
    metadata_id = None;
    metadata_size = None;
}

let id t = Peer.id t.peer

let to_string t = id t |> Peer_id.to_readable_string  

let get_pending t = Int.Set.to_list t.pending

(* must be call right after handshake *)
let init_size_owned_pieces t num_piece = t.have <- Bitset.empty num_piece

let has_piece t i = Bitset.belongs t.have i

let owned_pieces t = t.have

let set_owned_piece t i = Bitset.insert t.have i

let set_owned_pieces t s = Bitset.insert_from_bitfield t.have s;
  info !"peer %{} has %d pieces" t (Bitset.card t.have) 

let is_or_not b = if b then "" else "not"

let set_peer_interested t b = 
  info !"%{} is %{is_or_not} interested" t b;
  t.peer_interested <- b

let set_peer_choking t b = 
  info !"%{} is %{is_or_not} choking" t b;
  t.peer_choking <- b

let set_am_interested t b = 
  info !"I am %{is_or_not} interested in %{}" b t;
  t.am_interested <- b

let set_am_choking t b = 
  info !"I am %{is_or_not} choking %{}" b t;
  t.am_choking <- b

let pending_size t = Int.Set.length t.pending

let has_pending t = not (Int.Set.is_empty t.pending)

let clear_pending t = t.pending <- Int.Set.empty 

let remove_pending t i = t.pending <- Int.Set.remove t.pending i

let add_pending t i = t.pending <- Int.Set.add t.pending i

let iter_pending t ~f = Int.Set.iter t.pending ~f