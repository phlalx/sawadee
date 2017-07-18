open Core
open Async
open Log.Global
module G = Global

type t = {
  index : int;
  mutable status : [ `Requested | `Downloaded | `Not_requested | `On_disk ];
  hash : Bt_hash.t;
  length : int;
  content : string; (* TODO could be a substring *)
  blocks : Bitset.t;
} 

let create ~index hash ~len = 
  let num_blocks = (len + G.block_size - 1) / G.block_size in
  { index; status = `Not_requested; length = len; hash; content = String.create len; 
    blocks = Bitset.empty num_blocks }

let get_content t ~off ~len = assert false

let get_status t = t.status

let set_status t st = t.status <- st

let length t = t.length

let get_index t = t.index

let to_be_downloaded t = t.status = `Not_requested

let set_requested t = 
  assert (t.status = `Not_requested);
  t.status <- `Requested

let set_not_requested t = 
  (* assert (t.status = `Requested); *)
  if not (t.status = `Downloaded) then
    t.status <- `Not_requested

let num_blocks t = (t.length + G.block_size - 1) / G.block_size

let block_length t off = min (t.length - off) G.block_size 

let iter t ~f = 
  for i = 0 to (num_blocks t) - 1 do 
    let off = i * G.block_size in
    let len = block_length t off in
    f ~index:t.index ~off ~len 
  done

(* TODO this look a bit ugly *)
let update t ~off (block:string) = 
  let index = 
    assert (off % G.block_size = 0);  (* TODO other error for that *)
    off / G.block_size in
  let len = String.length block in
  assert (len = (block_length t off));
  Bitset.insert t.blocks index;
  String.blit ~src:block ~src_pos:0 ~dst:t.content ~dst_pos:off ~len;
  if Bitset.is_full t.blocks then ( 
    let hash_piece = Sha1.to_bin (Sha1.string t.content) in 
    if (hash_piece = Bt_hash.to_string t.hash) then (
      `Downloaded )
    else  (
      debug "Hash not equals";
      Bitset.reset t.blocks;
      `Hash_error
    )  
  ) else ( 
    `Ok 
  )

let is_downloaded t = t.status = `Downloaded

let to_string t = sprintf "(i = %d len = %d)" t.index t.length 
