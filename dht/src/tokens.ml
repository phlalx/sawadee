open Core

type token = string

type t = (Node_id.t, (token * Addr.t)) Hashtbl.t

let add t id v = Hashtbl.add t id v |> ignore 

let create () = Hashtbl.Poly.create ()

let iter = Hashtbl.iter