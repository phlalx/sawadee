open Core

module Be = Bencode_ext

type t = Node_id.t * Addr.t  

let to_compact (n, p) = (Node_id.to_string n) ^ (Addr.to_compact p)

let of_compact s = 
  let s1 = String.sub s ~pos:0 ~len:Node_id.length in
  let s2 = String.sub s ~pos:Node_id.length ~len:6 in 
  (Node_id.of_string s1, Addr.of_compact s2) 

let list_to_bencode nis = 
  let f acc p = acc ^ (to_compact p) in Be.String (List.fold nis ~init:"" ~f)

let length = Node_id.length + Addr.length

let of_bencode b = Be.as_string_exn b |> of_compact

let list_of_bencode b = Be.split b length |> List.map ~f:of_bencode

let to_string (n, p) = sprintf !"%{Node_id.to_hex} %{Addr}" n p

let of_string_exn s =
  match String.split s ~on:' ' with 
  | [n; a] -> (Node_id.of_hex n), (Addr.of_string a) 
  | _ -> failwith "wrong node info format"

let list_to_string t =
  List.map t ~f:to_string |> String.concat ~sep:"\n"

let list_of_string_exn s = 
  String.split_lines s |> List.map ~f:of_string_exn