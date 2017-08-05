open Core

include Hash_id.Id 

let num_bits x = 
let rec num_bits_aux c acc i = 
  if i = 8 then
    acc
  else 
    num_bits_aux (c/2) (acc + (c % 2)) (i+1)
in num_bits_aux x 0 0 

let dist s1 s2 = 
  let l1 = String.to_list s1 in 
  let l2 = String.to_list s2 in 
  let f x y = num_bits ((int_of_char x) lxor (int_of_char y)) in
  let l3 = List.map2_exn l1 l2 ~f in
  List.fold ~init:0 ~f:(fun x acc -> x + acc) l3

let distance t1 t2 = dist t1 t2 

let distance_hash t h = dist t (Bt_hash.to_string h)

let compare h n1 n2 = (distance_hash n1 h) - (distance_hash n2 h)
