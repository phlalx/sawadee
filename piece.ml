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

let content p = p.content

let length p = p.length

let get_index p = p.index

let to_be_downloaded p = p.status = `Not_requested

let set_requested p = 
  assert (p.status = `Not_requested);
  p.status <- `Requested

let create ~index ~hash ~len = 
  let num_blocks = (len + block_size_int - 1) / block_size_int in
  { index; status = `Not_requested; length = len; hash; content = String.create len; 
    blocks = Bitset.create num_blocks }

let num_blocks piece = (piece.length + block_size_int - 1) / block_size_int

let offset_to_index off =
  let off_int = Int32.to_int_exn off in 
  assert (off_int % block_size_int = 0);
  off_int / block_size_int

let offset_length piece index =
  let len = piece.length in
  let offset = index * block_size_int in
  let block_len = min (len - offset) block_size_int in
  Int32.of_int_exn offset, Int32.of_int_exn block_len

let update (p:t) (index:int) (block:string) = 
  let (offset, block_length) = offset_length p index in
  let offset_int = Int32.to_int_exn offset in 
  let block_length_int = Int32.to_int_exn block_length in 
  let len = String.length block in
  assert (len = block_length_int);
  Bitset.set p.blocks index true;
  String.blit ~src:block ~src_pos:0 ~dst:p.content ~dst_pos:offset_int ~len;
  if Bitset.is_one p.blocks then ( 
    let hash_piece = Sha1.to_bin (Sha1.string p.content) in 
    if (hash_piece = p.hash) then (
      p.status <- `Downloaded;
      `Downloaded )
    else  (
      debug "Hash not equals %S %S" hash_piece p.hash;
       Bitset.clear p.blocks;
       p.status <- `Not_requested;
       `Hash_error
    )  
  ) else ( 
    `Ok
  )

let blocks p = 
  let (num_blocks:int) = (p.length + block_size_int - 1) / block_size_int  in
  let len = Int32.of_int_exn p.length in
  let rec block_aux p offset =
    let open Int32 in
    if offset + block_size <= len then
      (offset, block_size) :: block_aux p (offset + block_size)
    else if offset = len then
      []
    else 
      [(offset, len - offset)]
  in 
  let res = block_aux p 0l in
  let (l:int) = List.length res in
  assert (l = num_blocks); 
  res