open Core

type t = 
| Leaf of int
| Node of t * t
[@@deriving sexp]

let tree = Node (Leaf(1), (Leaf(2)))

let sexp_tr = sexp_of_t tree 
let str = Sexp.to_string sexp_tr
let _ = print_string str