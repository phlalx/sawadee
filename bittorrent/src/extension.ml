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

let handshake_of_bin b bm =
  try 
    let metadata_size = dict_get_suff_int_exn b ~suffix:"metadata_size" in
    let metadata = dict_get_suff_int_exn bm ~suffix:"metadata" in 
    Handshake [ `Metadata (metadata, metadata_size) ]
  with 
    _ -> Unknown

let meta_of_bin b s =  
  try
    let msg_type = B.dict_get_int_exn b "msg_type" in
    let piece = B.dict_get_int_exn b "piece" in
    let n = String.length s in 
    match msg_type with 
    | 0 -> failwith "not implemented yet"
    | 1 -> 
      (* let total_size = B.dict_get_int_exn b "total_size" in *)
      Data (piece, s)
    | 2 -> 
      Reject piece
    | _ -> Unknown
  with
  | _ -> Unknown


let of_bin s =
  let (b, s') = B.decode_beginning_exn s in
  let n = String.length s' in
  info !"Extension: decoding message with %d trailing bytes" n;
  debug  !"Extension: decoding %{B.pretty_print} " b;
  match B.dict_get b "m" with
  | None -> 
    meta_of_bin b s'
  | Some bm -> 
    assert (n = 0);
    handshake_of_bin b bm 

let to_string = function
  | Request i -> sprintf "request(%d)" i
  | Reject i -> sprintf "reject(%d)" i
  | Data (i, s) -> sprintf "data(%d, %d)" i (String.length s)
  | Handshake ext -> sprintf !"Handshake [%{ext_to_string}]" ext 
  | Unknown -> "unknown"
