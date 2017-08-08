(* generic 20 bytes string identifier *)

open Core

module type ID = sig

  type t

  val bin_t : t Bin_prot.Type_class.t

  val of_string : string -> t

  val to_string : t -> string

  val dummy : t 

  val random : unit -> t

  val length : int

  val to_string_hum : t -> string

  val to_bencode : t -> Bencode_ext.t

  val of_bencode : Bencode_ext.t -> t

  val list_to_bencode : t list -> Bencode_ext.t

  val list_of_bencode : Bencode_ext.t -> t list

  val to_hex : t -> string

  val of_hex : string -> t

end

module Id = struct
  open Core

  module B = Bencode_ext

  type t = string
  [@@deriving bin_io]

  let of_string x = assert ((String.length x) = 20); x 

  let to_string x = x

  let length = 20

  let _ = Random.self_init ()

  let random () = 
    String.init length ~f:(fun _ -> char_of_int (Random.int 256))

  let dummy = "????????????????????"

  let to_bencode t = B.String t

  let of_bencode = B.as_string_exn

  let to_string_hum x =
    let f c =
      let i = int_of_char c in
      char_of_int (65 + (i % 26))
    in String.suffix (String.map x ~f) 6 

  let list_to_bencode l = B.String (String.concat (List.map l ~f:to_string))

  let list_of_bencode b = B.split b length |> List.map ~f:of_bencode

  let to_hex t = Hex.of_string t |> function (`Hex s) -> s

  let of_hex s = Hex.to_string (`Hex s)

end