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

  val to_bencode : t -> Bencode_ext.t

  val of_bencode : Bencode_ext.t -> t

  val list_to_bencode : t list -> Bencode_ext.t

  val list_of_bencode : Bencode_ext.t -> t list

end

module Id = struct

  module B = Bencode_ext

  type t = string

  let of_string x = x 

  let to_string x = x

  let length = 20

  let _ = Random.self_init ()

  let random () = 
    String.init length ~f:(fun _ -> char_of_int (Random.int 256))

  let dummy = "????????????????????"

  let to_bencode t = B.String t

  let of_bencode = B.as_string_exn

  let to_readable_string x =
    let f c =
      let i = int_of_char c in
      char_of_int (65 + (i % 26))
    in String.prefix (String.map x ~f) 5

  let list_to_bencode l = B.String (String.concat (List.map l ~f:to_string))

  let list_of_bencode b = B.split b length |> List.map ~f:of_bencode

end