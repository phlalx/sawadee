open Core

type t = string

let random () = String.init 20 ~f:(fun _ -> char_of_int (Random.int 255))

let to_string x = x

let of_string x = assert (String.length x = 20); x