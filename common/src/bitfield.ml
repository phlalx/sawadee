open Core
(* open Sexplib.Std *)

type t = string
[@@deriving sexp, bin_io] 

let length = String.length

let length_from_size i = (i + 7) / 8

let to_string x = x

let of_string x = x  

let validate x num = assert false (* TODO *)

let empty n = 
  let l = length_from_size n in 
  String.make l '\000'

(* TODO make these two function look the same *)
(** get/set i-th bit from a string. Bit 0 is left-most *)
let get t i =
  let byte_index = i / 8 in
  let bit = 7 - (i mod 8) in
  let i = t.[byte_index] |> int_of_char in
  ((i lsr bit) % 2) = 1

let set t i b = 
  let byte_index = i / 8 in
  let bit = 7 - (i mod 8) in
  let i = t.[byte_index] |> int_of_char in 
  if b then
    t.[byte_index] <- i lor (1 lsl bit) |> char_of_int 
  else  
    t.[byte_index] <- i land (lnot (1 lsl bit)) |> char_of_int 

let copy ~src ~dst = 
  let len = String.length src in
  String.blit ~src ~dst ~src_pos:0 ~dst_pos:0 ~len 

let count_bits n = 
  let x = ref n in
  let res = ref 0 in
  for i = 0 to 7 do 
    if (!x % 2 = 1) then
      incr res;
    x := !x / 2
  done;
  !res

let card t = 
  let num_bits c = int_of_char c |> count_bits in 
  let f acc c = acc + (num_bits c) in
  String.fold t ~init:0 ~f 

let rec to_list_aux t n res b =
  match n, (b = (get t (n-1))) with
  | 1, true -> 0 :: res
  | 1, false -> res 
  | _, true -> to_list_aux t (n-1) ((n-1) :: res) b
  | _, false -> to_list_aux t (n-1) res b

let to_list t n =
  assert (n >= 1);
  to_list_aux t n [] true 

let to_compl_list t n =
  assert (n >= 1);
  to_list_aux t n [] false

let to_string_hum t n =  to_list t n |> List.to_string ~f:string_of_int 







