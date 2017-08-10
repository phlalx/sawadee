open Core

type t = (int, Peer.t list) Hashtbl.t

(* create with the list of pieces I want *)
let create () = Hashtbl.Poly.create ()

let init t l max_pending = ()

let remove_piece t i = ()

let set_peer_choking t p b = ()

(* peer i has pieces l *)  
let add t l p = ()

let get_request () = assert false 

let remove_peer t p = ()




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