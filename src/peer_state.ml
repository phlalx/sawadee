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
  mutable id : Peer_id.t;
  mutable idle : bool;
}

let create peer = {
    peer;
    id = Peer_id.dummy;
    peer_interested = false; 
    peer_choking = true; 
    am_interested = true; (* should be opposite when starting *)
    am_choking = false; (* should be opposite when starting TODO *) 
    have = Bitset.empty 0; (* to be set by [init_size_owned_pieces] *)
    pending = Int.Set.empty;
    idle = false;
}

let id t = t.id

let peer t = t.peer

let is_idle t = t.idle

let set_idle t b = t.idle <- b

let to_string t = Peer_id.to_readable_string t.id 

let get_pending t = Int.Set.to_list t.pending

(* must be call right after handshake *)
let init_size_owned_pieces t num_piece = t.have <- Bitset.empty num_piece

let has_piece t i = Bitset.belongs t.have i

let owned_pieces t = t.have

let set_owned_piece t i = Bitset.insert t.have i

let set_owned_pieces t s = Bitset.insert_from_bitfield t.have s;
  info "peer %s has %d pieces" (to_string t) (Bitset.card t.have) 

let is_or_not b = if b then "" else "not"

let set_peer_interested t b = 
  info "%s is %s interested" (to_string t) (is_or_not b);
  t.peer_interested <- b

let set_peer_choking t b = 
  info "%s is %s choking" (to_string t) (is_or_not b);
  t.peer_choking <- b

let set_am_interested t b = 
  info "I am %s interested in %s" (is_or_not b) (to_string t);
  t.am_interested <- b

let set_am_choking t b = 
  info "I am %s choking %s" (is_or_not b) (to_string t);
  t.am_choking <- b

let is_peer_choking t = t.peer_choking

let is_peer_interested t = t.peer_interested

let am_choking t = t.am_choking

let am_interested t = t.am_interested

let pending_size t = Int.Set.length t.pending

let has_pending t = not (Int.Set.is_empty t.pending)

let clear_pending t = t.pending <- Int.Set.empty 

let remove_pending t i = t.pending <- Int.Set.remove t.pending i

let add_pending t i = t.pending <- Int.Set.add t.pending i

let iter_pending t ~f = Int.Set.iter t.pending ~f