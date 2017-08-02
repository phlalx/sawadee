open Core
open Async
open Log.Global

module B = Bencode_ext

type t = 
  | Reject 
  | Data of string 
  | Handshake of int * int
  | Unknown

let to_string t = 
  let bc = 
    match t with
    | Unknown -> B.Dict [ "m", B.Dict [] ]
    | _ -> assert false
  in 
  B.encode_to_string bc

let of_string s = 
  let bc = `String s |> B.decode in 
  info "received extended message %s" (B.pretty_print bc);
  let d = B.dict_get_exn bc "m" in 
  let _d' = B.as_dict_exn d in
  Unknown

