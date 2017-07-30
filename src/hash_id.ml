(* generic 20 bytes string identifier *)

open Core

module type ID = sig

  type t

  val of_string : string -> t

  val to_string : t -> string

  val dummy : t 

  val random : unit -> t

  val length : int

  val to_readable_string : t -> string

end

module Id = struct

  type t = string

  let of_string x = x 

  let to_string x = x

  let length = 20

  let peer_id_length = 20

  let _ = Random.self_init ()

  let random () = 
    String.init peer_id_length ~f:(fun _ -> char_of_int (Random.int 256))

  let dummy = "????????????????????"

  let to_readable_string x =
    let f c =
      let i = int_of_char c in
      char_of_int (65 + (i % 26))
    in String.prefix (String.map x ~f) 5

end