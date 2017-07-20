open Core

exception Bencode_error

let get x =
  match x with
  | Some y -> y
  | None -> raise Bencode_error

let split (s:string) split_size =
  let n = String.length s in
  assert (n % split_size = 0);
  let f i = String.sub s (i * split_size) split_size in
  Array.init (n / split_size) ~f