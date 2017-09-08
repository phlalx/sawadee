
include Bencode

open Core

let value_exn b = 
  function 
  | Some v -> v 
  | None -> failwith (pretty_print b)


let as_string_exn x = as_string x |> Option.value_exn

let as_int_exn x = as_int x |> Option.value_exn

let as_list_exn x = as_list x |> Option.value_exn

let as_dict_exn x = as_dict x |> Option.value_exn

let dict_get_exn b s = dict_get b s |> value_exn b

let dict_get_string_exn b s =
  dict_get_exn b s |> as_string_exn

let dict_get_int_exn b s =
  dict_get_exn b s |> as_int_exn

let dict_get_list_exn b s =
  dict_get_exn b s |> as_list_exn

let split_array (s:string) split_size =
  let n = String.length s in
  assert (n % split_size = 0);
  let f i = String.sub s (i * split_size) split_size in
  Array.init (n / split_size) ~f

let split_list split_size (s:string) =
  Array.to_list (split_array s split_size)

let split b s = 
  as_string_exn b 
  |> split_list s 
  |> List.map ~f:(fun x -> String x) 
