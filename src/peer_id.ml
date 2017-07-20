open Core

type t = string

let peer_id_length = 20

let random () = 
  String.init peer_id_length ~f:(fun _ -> char_of_int (Random.int 255))

let dummy = String.create peer_id_length

let to_string x = x

let of_string x = assert (String.length x = peer_id_length); x