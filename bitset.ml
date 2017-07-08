(** quick and dirty implementation using array of bool 

  TODO: use existing API *)
open Core

type t = bool Array.t

let length = Array.length

let create n = Array.create n false

let get x i = x.(i)

let set x i b = x.(i) <- b

let fill_from_string (bits:string) (have:bool Array.t) =
  let get_bit i bits =
    let byte_num = i / 8 in
    let bit = i mod 8 in
    let c = String.get bits byte_num in
    let i = int_of_char c in
    ((i lsr bit) % 2) = 1
  in
  for i = 0 to (Array.length have) - 1 do
    have.(i) <- get_bit i bits
  done

let to_string x : string =
  let n = Array.length x in
  let s_len = if n % 8 = 0 then n / 8 else ((n / 8) + 1) in
  let res = Bytes.create s_len in
  for i = 0 to s_len - 1 do
    let byte = ref 0 in
    for u = 0 to 7 do 
      let b = if x.(i * 8 + u) then 1 else 0 in
      byte := !byte lxor b;
      byte := !byte * 2;
    done;
    String.set res i (char_of_int !byte);
  done;
  res
