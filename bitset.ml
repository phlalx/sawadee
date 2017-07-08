(** quick and dirty implementation using array of bool 

  TODO: use existing API instead *)

open Core

type t = 
{
  bits : bool Array.t;
  mutable num_set : int ;
}

let length x = Array.length x.bits

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

let is_one x = x.num_set = Array.length x.bits

(* TODO check this - rewrite *)
let fill_from_string (bits:string) (have:t) =
  let get_bit i bits =
    let byte_num = i / 8 in
    let bit = i mod 8 in
    let c = String.get bits byte_num in
    let i = int_of_char c in
    ((i lsr bit) % 2) = 1
  in
  for i = 0 to (Array.length have.bits) - 1 do
    have.bits.(i) <- get_bit i bits
  done

(* TODO check this - rewrite *)
let to_string x : string =
  let n = Array.length x.bits in
  let s_len = if n % 8 = 0 then n / 8 else ((n / 8) + 1) in
  let res = Bytes.create s_len in
  for i = 0 to s_len - 1 do
    let byte = ref 0 in
    for u = 0 to 7 do 
      let b = if x.bits.(i * 8 + u) then 1 else 0 in
      byte := !byte lxor b;
      byte := !byte * 2;
    done;
    String.set res i (char_of_int !byte);
  done;
  res
