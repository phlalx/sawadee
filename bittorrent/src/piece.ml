open Core
open Async
open Log.Global
module G = Global

type t = {
  index : int;
  hash : Bt_hash.t;
  length : int;
  content : Bigsubstring.t;
  blocks : bool Array.t;
} 

let create ~pos ~index ~len hash bs = 
  let num_blocks = (len + G.block_size - 1) / G.block_size in
  { index; length = len; hash;  
    blocks = Array.create num_blocks false;
    content = Bigsubstring.create ~pos ~len bs; }

let get_content t ~off ~len = 
  let b = Bigsubstring.base t.content in
  let p = Bigsubstring.pos t.content in
  Bigstring.to_string b ~pos:(p + off) ~len

let get_bigstring_content t = t.content

let length t = t.length

let get_index t = t.index

let num_blocks t = (t.length + G.block_size - 1) / G.block_size

(* the last block can have a different size *)
let block_length t ~off = min (t.length - off) G.block_size 

let is_valid_block_request t ~off ~len = 
  off + len <= t.length && len <= G.max_block_size

let blocks t = 
  let f i =  
    let off = i * G.block_size in
    let len = block_length t off in
    match t.blocks.(i) with
    | true -> None
    | false -> Some Block.{ piece = t.index; off; len}
  in
  List.range 0 (num_blocks t) |> List.filter_map ~f

let is_hash_ok t =
  let hash_piece = 
    Bigsubstring.to_string t.content |> Sha1.string |> Sha1.to_bin 
  in 
  hash_piece = Bt_hash.to_string t.hash

let update t ~off (block:string) = 
  let index = off / G.block_size in
  let len = String.length block in
  t.blocks.(index) <- true;
  let base = Bigsubstring.base t.content in 
  let off = off + Bigsubstring.pos t.content in
  Bigstring.From_string.blit ~src:block ~src_pos:0 ~dst:base ~dst_pos:off ~len;
  match Array.for_all t.blocks ident with  
  | true ->
    if is_hash_ok t then 
      `Downloaded 
    else  (
      debug "Hash not equals";
      Array.fill t.blocks ~pos:0 ~len:(Array.length t.blocks) false;
      `Hash_error)
  | false ->  `Ok 

let to_string t = sprintf "(i = %d len = %d)" t.index t.length 

