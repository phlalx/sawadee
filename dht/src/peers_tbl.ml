open Core

type t = (Node_id.t, Addr.t list) Hashtbl.t

let create () = Hashtbl.Poly.create ()

let add t n a = 
  let insert a l = if List.mem ~equal:(=) l a then l else a :: l in 
  let f o = 
    let l = Option.value ~default:[] o in Some (insert a l)
  in 
  Hashtbl.change t n ~f

let find t n = Hashtbl.find t n |> Option.value ~default:[]