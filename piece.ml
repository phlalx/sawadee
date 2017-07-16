open Core
open Async
open Log.Global

let block_size = 32768

type t = {
  index : int;
  mutable status : [ `Requested | `Downloaded | `Not_requested | `On_disk ];
  hash : Bt_hash.t;
  length : int;
  content : string; (* TODO could be a substring *)
  blocks : Bitset.t;
} 

let create ~index hash ~len = 
  let num_blocks = (len + block_size - 1) / block_size in
  { index; status = `Not_requested; length = len; hash; content = String.create len; 
    blocks = Bitset.empty num_blocks }

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

let num_blocks t = (t.length + block_size - 1) / block_size

let block_length t off = min (t.length - off) block_size 

let offset_length t index =
  let len = t.length in
  let offset = index * block_size in
  let block_len = min (len - offset) block_size in
  offset, block_len

let iter t ~f = 
  for i = 0 to (num_blocks t) - 1 do 
    let off = i * block_size in
    let len = block_length t off in
    f ~index:t.index ~off ~len
  done


(* TODO this look a bit ugly *)
let update t ~off (block:string) = 
  let index = 
    assert (off % block_size = 0);  (* TODO other error for that *)
    off / block_size in
  let len = String.length block in
  assert (len = (block_length t off));
  Bitset.insert t.blocks index;
  String.blit ~src:block ~src_pos:0 ~dst:t.content ~dst_pos:off ~len;
  if Bitset.is_full t.blocks then ( 
    let hash_piece = Sha1.to_bin (Sha1.string t.content) in 
    if (hash_piece = Bt_hash.to_string t.hash) then (
      t.status <- `Downloaded;
      `Downloaded )
    else  (
      debug "Hash not equals";
      Bitset.reset t.blocks;
      t.status <- `Not_requested;
      `Hash_error
    )  
  ) else ( 
    `Ok 
  )

let read t fd =
  set_status t `On_disk;
  let rd = Reader.create fd in
  let file_offset = Int64.of_int (t.index * t.length) in
  Async_unix.Unix_syscalls.lseek fd ~mode:`Set file_offset
  >>= fun off -> 
  assert (off = file_offset); (* TODO fail silently *)
  Reader.read rd t.content 
  >>| function
  | `Eof -> assert false
  | `Ok _ -> ()

let write t fd =
  if get_status t = `Downloaded then (
    assert (Bitset.is_full t.blocks); 
    assert ((String.length t.content) = t.length);
    let wr = Writer.create fd in
    let file_offset = Int64.of_int (t.index * t.length) in
    Async_unix.Unix_syscalls.lseek fd ~mode:`Set file_offset
    >>| fun off -> 
    assert (off = file_offset); (* TODO fail silently *)
    Writer.write wr t.content;
    set_status t `On_disk )
  else
    return ()

let is_downloaded t = t.status = `Downloaded

let to_string t = sprintf "(i = %d len = %d)" t.index t.length 
