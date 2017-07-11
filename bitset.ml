(** quick implementation using an array of bool  *)
open Core

type t = 
{
  bits : bool Array.t;
  mutable num_set : int ;
}

let length t = Array.length t.bits

let clear t = 
  t.num_set <- 0;
  Array.fill t.bits 0 (length t) false

let create n = {
  bits = Array.create n false;
  num_set = 0;
}

let get t i = t.bits.(i)

let set t i b = 
  if not t.bits.(i) && b then ( 
    t.num_set <- t.num_set + 1
  ) else if t.bits.(i) && not b then ( 
    t.num_set <- t.num_set - 1
  );
  t.bits.(i) <- b

let is_one t = t.num_set = length t

let num_bit_set t = t.num_set

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

let fill_from_string t (bits:string) =
  for i = 0 to (length t) - 1 do
    t.bits.(i) <- get_bit bits i
  done

let to_string t =
  let n = length t in
  let s_len = (n + 7) / 8 in
  let res = String.make s_len '\000' in
  for i = 0 to n - 1 do
    if get t i then set_bit res i
  done;
  res









