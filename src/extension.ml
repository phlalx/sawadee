open Core
open Async
open Log.Global

module B = Bencode_ext

type ext = [ `Metadata of int * int ]

let ext_to_string ext = 
  let f = function 
    | `Metadata (i, s) -> sprintf "metadata %d %d" i s
  in List.map ~f ext |> String.concat ~sep:"|" 

type t = 
  | Reject of int  
  | Request of int 
  | Data of int * string 
  | Handshake of ext list 
  | Unknown

let extension_message t p = 
  B.Dict [ "msg_type", B.Integer t; "piece", B.Integer p ] 

let to_bin t = 
  (match t with
   | Unknown -> assert false
   | Request i -> 
     extension_message 0 i |> B.encode_to_string 
   | Data (i, s) -> 
     let b = extension_message 1 i |> B.encode_to_string in b ^ s
   | Reject i -> 
     extension_message 2 i |> B.encode_to_string 
   | Handshake [] -> 
     B.Dict [ "m", B.Dict [] ] |> B.encode_to_string  
   | _ -> assert false (* TODO: support other types *)
  )

exception Unknown_message

let dict_get_suff_int_exn (d : B.t) ~suffix : int =
  let f (x, _) = String.is_suffix x ~suffix in 
  match B.as_dict_exn d |> List.filter ~f with
  | [(_, v) ] -> B.as_int_exn v
  |  _ -> raise Unknown_message  

let metadata_ext_of_bin s =
  let (bc, s) = B.decode_beginning_exn s in
  info !"received extended message %{B.pretty_print}" bc; 
  assert false

let handshake_of_bin s =
  let bc = `String s |> B.decode in
  info !"received extended message %{B.pretty_print}" bc; 
  try 
    let metadata_size = dict_get_suff_int_exn bc ~suffix:"metadata_size" in
    let metadata = 
      B.dict_get_exn bc "m" |> dict_get_suff_int_exn ~suffix:"metadata"  in 
    Handshake [ `Metadata (metadata, metadata_size) ]
  with 
    _ -> Handshake []

let of_bin kind s = 
  match kind with 
  | `Metadata_ext -> metadata_ext_of_bin s 
  | `Handshake -> handshake_of_bin s

let to_string = function
  | Request i -> sprintf "request %d" i
  | Reject i -> sprintf "reject %d" i
  | Data (i, _) -> sprintf "data %d" i
  | Handshake ext -> sprintf !"Handshake [%{ext_to_string}]" ext 
  | Unknown -> "unknown"
