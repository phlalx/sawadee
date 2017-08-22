
include Hash_id.Id

open Core

let sha1_of_string s = Sha1.string s |> Sha1.to_bin 

let to_string_hum = to_hex

let sexp_of_t x = Sexp.Atom (to_string_hum x)