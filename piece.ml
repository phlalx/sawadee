open Core
open Async
open Log.Global

let block_size = 32768l
let block_size_int = Int32.to_int_exn block_size

type t = {
  index : int;
  mutable status : [ `Requested | `Downloaded | `Not_requested];
  hash : string;
  length : int;
  content : string;
  blocks : Bitset.t
}

let content t = t.content

let length t = t.length

let get_index t = t.index

let to_be_downloaded t = t.status = `Not_requested

let set_requested t = 
  assert (t.status = `Not_requested);
  t.status <- `Requested

let set_not_requested t = 
  assert (t.status = `Requested);
  t.status <- `Not_requested

let create ~index ~hash ~len = 
  let num_blocks = (len + block_size_int - 1) / block_size_int in
  { index; status = `Not_requested; length = len; hash; content = String.create len; 
    blocks = Bitset.create num_blocks }

let num_blocks t = (t.length + block_size_int - 1) / block_size_int

let offset_to_index off =
  let off_int = Int32.to_int_exn off in 
  assert (off_int % block_size_int = 0);
  off_int / block_size_int

let offset_length t index =
  let len = t.length in
  let offset = index * block_size_int in
  let block_len = min (len - offset) block_size_int in
  Int32.of_int_exn offset, Int32.of_int_exn block_len

let update t (index:int) (block:string) = 
  let (offset, block_length) = offset_length t index in
  let offset_int = Int32.to_int_exn offset in 
  let block_length_int = Int32.to_int_exn block_length in 
  let len = String.length block in
  assert (len = block_length_int);
  Bitset.set t.blocks index true;
  String.blit ~src:block ~src_pos:0 ~dst:t.content ~dst_pos:offset_int ~len;
  if Bitset.is_one t.blocks then ( 
    let hash_piece = Sha1.to_bin (Sha1.string t.content) in 
    if (hash_piece = t.hash) then (
      t.status <- `Downloaded;
      `Downloaded )
    else  (
      debug "Hash not equals %S %S" hash_piece t.hash;
      Bitset.clear t.blocks;
      t.status <- `Not_requested;
      `Hash_error
    )  
  ) else ( 
    `Ok
  )

let blocks t = 
  let (num_blocks:int) = (t.length + block_size_int - 1) / block_size_int  in
  let len = Int32.of_int_exn t.length in
  let rec block_aux t offset =
    let open Int32 in
    if offset + block_size <= len then
      (offset, block_size) :: block_aux t (offset + block_size)
    else if offset = len then
      []
    else 
      [(offset, len - offset)]
  in 
  let res = block_aux t 0l in
  let (l:int) = List.length res in
  assert (l = num_blocks); 
  res