
include Bencode

open Core

let as_string_exn x = as_string x |> Option.value_exn

let as_int_exn x = as_int x |> Option.value_exn

let as_list_exn x = as_list x |> Option.value_exn

let as_dict_exn x = as_dict x |> Option.value_exn

let dict_get_exn b s = dict_get b s |> Option.value_exn

let get_string_from_dict_exn b s =
  dict_get_exn b s |> as_string_exn

let get_int_from_dict_exn b s =
  dict_get_exn b s |> as_int_exn

let get_list_from_dict_exn b s =
  dict_get_exn b s |> as_list_exn

