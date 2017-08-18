open Core
open Async
open Log.Global

module Be = Bencode_ext

type id = int 
[@@deriving sexp]

type bin = string
[@@deriving sexp]

type ext = [ `Metadata of id * int ]
[@@deriving sexp]

let ext_to_string ext = Sexp.to_string (sexp_of_ext ext)

type t = 
  | Reject of int  
  | Request of int 
  | Data of int * string sexp_opaque
  | Handshake of ext list 
  | Unknown
[@@deriving sexp]

let extension_message t p = 
  Be.Dict [ "msg_type", Be.Integer t; "piece", Be.Integer p ] 

let to_bin t = 
  (match t with
   | Unknown -> assert false
   | Request i -> 
     extension_message 0 i |> Be.encode_to_string 
   | Data (i, s) -> 
     let b = extension_message 1 i |> Be.encode_to_string in b ^ s
   | Reject i -> 
     extension_message 2 i |> Be.encode_to_string 
   | Handshake [] -> 
     Be.Dict [ "m", Be.Dict [] ] |> Be.encode_to_string  
   | _ -> assert false (* TODO: support other types *)
  )

exception Unknown_message

let dict_get_suff_int_exn (d : Be.t) ~suffix : int =
  let f (x, _) = String.is_suffix x ~suffix in 
  match Be.as_dict_exn d |> List.filter ~f with
  | [(_, v) ] -> Be.as_int_exn v
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
    let msg_type = Be.dict_get_int_exn b "msg_type" in
    let piece = Be.dict_get_int_exn b "piece" in
    match msg_type with 
    | 0 -> failwith "not implemented yet"
    | 1 -> 
      Data (piece, s)
    | 2 -> 
      Reject piece
    | _ -> Unknown
  with
  | _ -> Unknown

let of_bin s =
  let (b, s') = Be.decode_beginning_exn s in
  let n = String.length s' in
  debug !"Extension: decoding %{Be.pretty_print} (%d trailing bytes)" b n;
  match Be.dict_get b "m" with
  | None -> 
    meta_of_bin b s'
  | Some bm -> 
    assert (n = 0);
    handshake_of_bin b bm 

let to_string t = Sexp.to_string (sexp_of_t t)
