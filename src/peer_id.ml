open Core

type t = string

let length = 20

let peer_id_length = 20

let _ = Random.self_init ()

let random () = 
  String.init peer_id_length ~f:(fun _ -> char_of_int (Random.int 256))

let dummy = "unkown-peer-id------"

let to_string x = x

let of_string x = assert (String.length x = peer_id_length); x

let to_readable_string x =
  let f c =
    let i = int_of_char c in
    char_of_int (65 + (i % 26))
  in String.prefix (String.map x ~f) 5