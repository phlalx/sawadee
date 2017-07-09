(** quick implementation using an array of bool  *)
open Core

type t = 
{
  bits : bool Array.t;
  mutable num_set : int ;
}

let length x = Array.length x.bits

let clear x = 
  x.num_set <- 0;
  Array.fill x.bits 0 (length x) false

let create n = {
  bits = Array.create n false;
  num_set = 0;
}

let get x i = x.bits.(i)

let set x i b = 
  if not x.bits.(i) && b then ( 
    x.num_set <- x.num_set + 1
  ) else if x.bits.(i) && not b then ( 
    x.num_set <- x.num_set - 1
  );
  x.bits.(i) <- b

let is_one x = x.num_set = length x

(** get i-th bit from a string. Bit 0 is left-most *)
let get_bit (bits:string) i =
  let byte_num = i / 8 in
  let bit = i mod 8 in
  let c = String.get bits byte_num in
  let i = int_of_char c in
  ((i lsr bit) % 2) = 1

(** set i-th bit from a string. Bit 0 is right-most *)
let set_bit (bits:string) i = 
  let byte_index = i / 8 in
  let by = int_of_char (bits.[byte_index]) in 
  let bit = 7 - (i mod 8) in
  let new_by = by lxor (1 lsl bit) in 
  bits.[byte_index] <- char_of_int new_by

let fill_from_string (x:t) (bits:string) =
  for i = 0 to (length x) - 1 do
    x.bits.(i) <- get_bit bits i
  done

let to_string x : string =
  let n = length x in
  let s_len = (n + 7) / 8 in
  let res = String.make s_len '\000' in
  for i = 0 to n - 1 do
    if get x i then set_bit res i
  done;
  res









