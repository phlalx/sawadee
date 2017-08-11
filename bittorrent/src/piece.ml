open Core
open Async
open Log.Global
module G = Global

type t = {
  index : int;
  hash : Bt_hash.t;
  length : int;
  content2 : Bigsubstring.t;
  content : Bigstring.t; 
  blocks : Bitset.t;
} 

let create ~pos ~index ~len hash bs = 
  let num_blocks = (len + G.block_size - 1) / G.block_size in
  { index; length = len; hash; content = Bigstring.create len; 
    blocks = Bitset.empty num_blocks; 
    content2 = Bigsubstring.create ~pos ~len bs; }

let get_content t ~off ~len = Bigstring.to_string ~pos:off ~len t.content

let get_bigstring_content t = t.content

let length t = t.length

let get_index t = t.index

let num_blocks t = (t.length + G.block_size - 1) / G.block_size

(* the last block can have a different size *)
let block_length t ~off = min (t.length - off) G.block_size 

(* TODO straighten these conditions off >=0 ...*)
let is_valid_block t ~off ~len =
  (off % G.block_size = 0) && (len = block_length t off)

let is_valid_block_request t ~off ~len = 
  off + len <= t.length && len <= G.max_block_size

let iter t ~f = 
  for i = 0 to (num_blocks t) - 1 do 
    let off = i * G.block_size in
    let len = block_length t off in
    f ~index:t.index ~off ~len 
  done

let is_hash_ok t =
  (* TODO see if there is a Sha1.bigstring *)
  let hash_piece = Sha1.to_bin (Sha1.string (Bigstring.to_string t.content)) in 
  hash_piece = Bt_hash.to_string t.hash

let update t ~off (block:string) = 
  let index = off / G.block_size in
  let len = String.length block in
  Bitset.insert t.blocks index;
  Bigstring.From_string.blit ~src:block ~src_pos:0 ~dst:t.content ~dst_pos:off 
    ~len;
  if Bitset.is_full t.blocks then ( 
    if is_hash_ok t then 
      `Downloaded 
    else  (
      debug "Hash not equals";
      Bitset.reset t.blocks;
      `Hash_error
    )  
  ) else ( 
    `Ok 
  )

let to_string t = sprintf "(i = %d len = %d)" t.index t.length 
