open Core

type t = string

let length = 20

let peer_id_length = 20

let random () = 
  String.init peer_id_length ~f:(fun _ -> char_of_int (65 + (Random.int 26)))

let dummy = "unkown-peer-id------"

let to_string x = x

let of_string x = assert (String.length x = peer_id_length); x