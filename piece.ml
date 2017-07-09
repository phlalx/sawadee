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
  content : string; (* TODO is string the right type? Buffer.t *)
  blocks : Bitset.t
}

let get_index p = p.index

let to_be_downloaded p = p.status = `Not_requested

let set_requested p = 
  assert (p.status = `Not_requested);
  p.status <- `Requested

let create index hash length = 
  let num_blocks = (length + block_size_int - 1) / block_size_int in
  { index; status = `Not_requested; length; hash; content = Bytes.create length; 
    blocks = Bitset.create num_blocks }

let update p o b = 
  let block_index = 
    let open Int32 in Int32.to_int_exn (o / block_size) in
  Bitset.set p.blocks block_index true; 
  (* TODO must be a function somewhere to do that *)
  for i = 0 to (String.length b) - 1 do 
    String.set p.content (i + (Int32.to_int_exn o)) (String.get b i)
  done;
  if Bitset.is_one p.blocks then ( 
    let sha_piece = Sha1.to_bin (Sha1.string p.content) in 
    if (sha_piece = p.hash) then (
      p.status <- `Downloaded;
      `Downloaded )
    else  (
       p.status <- `Not_requested;
       (* TODO reset bitset *)
      `Downloaded (* TODO should be `Sha_error, but for some reason, the
      hash never match *)
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