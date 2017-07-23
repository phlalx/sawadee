(* quick and not efficient implementation using an array of bool  *)

open Core

type t = {
  bits : bool Array.t;
  mutable num_set : int ;
}

let size t = Array.length t.bits

let reset t = 
  t.num_set <- 0;
  Array.fill t.bits 0 (size t) false

let count_bits bits : int = 
  let f x b = if b then x + 1 else x in
  Array.fold bits ~init:0 ~f

let create ~size:n ~f = 
  let bits = Array.init n f in
  let num_set = count_bits bits in
  { bits; num_set }

let empty ~size = {
  bits = Array.create size false;
  num_set = 0;
}

let to_list t = 
  let f i acc b = if b then i :: acc else acc in  
  Array.foldi t.bits ~init:[] ~f

let belongs t i = t.bits.(i)

let insert t i = 
  if not (belongs t i) then (
    t.num_set <- t.num_set + 1;
    t.bits.(i) <- true
  )

let is_full t = t.num_set = size t

let is_empty t = t.num_set = 0

let card t = t.num_set

let compl t = 
  let bits = Array.map t.bits ~f:not in 
  let num_set = (size t) - t.num_set in
  { bits; num_set }

(** get i-th bit from a string. Bit 0 is left-most *)
let get_bit (bits:string) i =
  let byte_num = i / 8 in
  let bit = i mod 8 in
  let c = String.get bits byte_num in
  let i = int_of_char c in
  ((i lsr (7 - bit)) % 2) = 1

(** set i-th bit from a string. Bit 0 is right-most *)
let set_bit (bits:string) i = 
  let byte_index = i / 8 in
  let by = int_of_char (bits.[byte_index]) in 
  let bit = 7 - (i mod 8) in
  let new_by = by lxor (1 lsl bit) in 
  bits.[byte_index] <- char_of_int new_by

let insert_from_bitfield t bf =
  let bits = Bitfield.to_string bf in
  for i = 0 to (size t) - 1 do
    t.bits.(i) <- get_bit bits i;
    if t.bits.(i) then t.num_set <- t.num_set + 1
  done

let inter t1 t2 = 
  let bits : bool Array.t = Array.map2_exn t1.bits t2.bits ~f:(&&) in
  let num_set = count_bits bits in
  { bits; num_set } 

let choose t : int option =
  match Array.findi t.bits ~f:(fun _ b -> b) with
  | None -> None
  | Some (i, _) -> Some i

let choose_random t : int option =
  let f i b = if b then Some i else None in
  let a = Array.filter_mapi t.bits ~f in
  let n = Array.length a in
  match n with
  | 0 -> None
  | _ -> Some a.(Random.int n) 

let bitfield_length t = ((size t) + 7) / 8  

let bitfield_length_from_size i = (i + 7) / 8

let to_string t =
    List.to_string ~f:string_of_int (to_list t)

let to_bitfield t = 
  let n = size t in
  let res = String.make (bitfield_length t) '\000' in
  for i = 0 to n - 1 do
    if belongs t i then set_bit res i
  done;
  Bitfield.of_string res

let of_bitfield bf num =
  let t = empty num in
  insert_from_bitfield t bf;
  t