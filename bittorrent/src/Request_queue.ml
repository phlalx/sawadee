
type t = (int, Peer.t list) Hashtbl.t

(* create with the list of pieces I want *)
let create _ = assert false

(* peer i has piece i*)  
let add i p = assert false

(* )

(* peer p leaves or choke *)
let leave p = assert false

(* give me at most n requests *)
let take n = assert false 



(* 
type table = (int, Peer.t list) Hashtbl.t
let add_peer_pieces table p =
  let bf = Peer.bitfield p |> (Bitfield.to_list 4444) in
  let upd = function 
    | None -> [p]
    | Some l -> p :: l 
  in
  List.iter bf ~f:(fun i -> Hashtbl.update table i ~f:upd)

 *)