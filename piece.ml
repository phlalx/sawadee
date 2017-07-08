open Core
open Async
open Log.Global

let block_size = 4096l
let block_size_int = Int32.to_int_exn block_size

type t = {
  index : int;
  mutable status : [ `Requested | `Downloaded | `Not_requested];
  hash : string;
  length : int;
  content : string;
  blocks : Bitset.t
}

let create index hash length = 
  let num_blocks = (length + block_size_int - 1) / block_size_int in
  { index; status = `Not_requested; length; hash; content = Bytes.create length; 
    blocks = Bitset.create num_blocks }

let update p o b = 
  let open Int32 in
  let block_index = Int32.to_int_exn (o / block_size) in
  Bitset.set p.blocks block_index true; 
  let o = Int32.to_int_exn o in
  let b_len = String.length b in
  (* debug "got piece begin = %d len = %d" o b_len; *)
  if Bitset.is_one p.blocks then ( 
    info "yeah! we managed to download a full piece!";
    p.status <- `Downloaded
  )  

let blocks p = 
  let (num_blocks:int) = (p.length + block_size_int - 1) / block_size_int  in
  let open Int32 in
  let len = Int32.of_int_exn p.length in
  let rec block_aux p offset =
    if offset + block_size <= len then
      (offset, block_size) :: block_aux p (offset + block_size)
    else if offset = len then
      []
    else 
      [(offset, len - offset)]
  in 
  let res = block_aux p 0l in
  let (l:int) = List.length res in
  assert (phys_equal l num_blocks); 
  res