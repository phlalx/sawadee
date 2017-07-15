open Core

type t = string

let random () = String.init 20 ~f:(fun _ -> char_of_int (Random.int 255))