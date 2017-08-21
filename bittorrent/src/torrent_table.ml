open Core

let t = Hashtbl.Poly.create ()

let add info_hash swarm = Hashtbl.add t ~key:info_hash ~data:swarm |> ignore

let has_hash = Hashtbl.mem t 

let find = Hashtbl.find t 

let find_exn = Hashtbl.find_exn t 

let keys () = Hashtbl.keys t

let data () = Hashtbl.data t