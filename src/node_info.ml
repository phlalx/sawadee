open Core
module B = Bencode_ext

type t = Node_id.t * Addr.t  

let to_compact (n, p) = (Node_id.to_string n) ^ (Addr.to_compact p)

let of_compact s = 
  let s1 = String.sub s ~pos:0 ~len:Node_id.length in
  let s2 = String.sub s ~pos:Node_id.length ~len:6 in 
  (Node_id.of_string s1, Addr.of_compact s2) 

let list_to_bencode nis = 
  let f acc p = acc ^ (to_compact p) in B.String (List.fold nis ~init:"" ~f)

let length = Node_id.length + Addr.length

let of_bencode b = B.as_string_exn b |> of_compact

let list_of_bencode b = B.split b length |> List.map ~f:of_bencode