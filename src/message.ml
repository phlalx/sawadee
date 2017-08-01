open Core
open Async
open Bin_prot
open Log.Global

type t =
  | KeepAlive
  | Choke
  | Unchoke
  | Interested
  | Not_interested
  | Have of int 
  | Bitfield of Bitfield.t
  | Request of int * int  * int
  | Piece of int * int * string
  | Cancel of int * int * int
  | Port of int
  (* TODO better to have Bencode.t instead of string *)
  | Extended of int * string 
[@@deriving sexp]

let max_size = Global.max_block_size + 13

(* 1 byte for type, and rest for payload *)
let size = function 
  | KeepAlive -> 0
  | Choke -> 1
  | Unchoke -> 1
  | Interested -> 1
  | Not_interested -> 1
  | Have _ -> 5
  | Bitfield s -> 1 + (Bitfield.length s) 
  | Request _ -> 13
  | Piece (_,_,s) -> 9 + (String.length s)
  | Cancel _ -> 13 
  | Port _ -> 3
  | Extended (_, b) -> 1 + (String.length b)

exception Unkown_message of int 

(* length is prefix-length, the number of bytes left to read, *)
let bin_read_payload buf ~pos_ref length =
  let x = Read.bin_read_char buf pos_ref in 
  match (int_of_char x) with
  | 0 -> Choke
  | 1 -> Unchoke
  | 2 -> Interested
  | 3 -> Not_interested
  | 4 -> let index = Read.bin_read_network32_int buf pos_ref in Have index
  | 5 -> 
    let bitfield_len = length - 1 in
    let bitfield = String.create bitfield_len in
    Common.blit_buf_string ~src_pos:!pos_ref buf bitfield ~len:bitfield_len;
    Bitfield (Bitfield.of_string bitfield)
  | 6 -> 
    let index = Read.bin_read_network32_int buf pos_ref in
    let begn = Read.bin_read_network32_int buf pos_ref in
    let len = Read.bin_read_network32_int buf pos_ref in
    Request (index, begn, len)
  | 7 -> 
    let block_len = length - 9 in
    let index = Read.bin_read_network32_int buf pos_ref in
    let begn = Read.bin_read_network32_int buf pos_ref in
    let block = String.create block_len in
    Common.blit_buf_string ~src_pos:!pos_ref buf block ~len:block_len; 
    Piece (index, begn, block)
  | 8 -> 
    let index = Read.bin_read_network32_int buf pos_ref in
    let begn = Read.bin_read_network32_int buf pos_ref in
    let len = Read.bin_read_network32_int buf pos_ref in
    Cancel (index, begn, len)
  | 9 -> 
    let port = Read.bin_read_network16_int buf pos_ref in
    assert (port >= 0);
    Port port
  | 20 ->  
    let b_len = length - 1 in
    let id = Read.bin_read_char buf pos_ref in
    let bencode = String.create b_len in
    Common.blit_buf_string ~src_pos:!pos_ref buf bencode ~len:b_len; 
    Extended (int_of_char id, bencode)
    
  | i -> raise (Unkown_message i)

let bin_read_t buf ~pos_ref = 
  let length = Read.bin_read_network32_int buf pos_ref in 
  match length with
  | 0 -> KeepAlive
  | length -> bin_read_payload buf ~pos_ref length

let bin_write_t (buf:Common.buf) ~(pos:Common.pos) (x:t) =
  match x with
  | KeepAlive -> Write.bin_write_network32_int buf pos 0
  | Choke -> 
    let pos = Write.bin_write_network32_int buf pos 1 in
    Write.bin_write_char buf pos (char_of_int 0) 
  | Unchoke ->
    let pos = Write.bin_write_network32_int buf pos 1 in
    Write.bin_write_char buf pos (char_of_int 1) 
  | Interested ->
    let pos = Write.bin_write_network32_int buf pos 1 in
    Write.bin_write_char buf pos (char_of_int 2) 
  | Not_interested ->
    let pos = Write.bin_write_network32_int buf pos 1 in
    Write.bin_write_char buf pos (char_of_int 3) 
  | Have index -> 
    let pos = Write.bin_write_network32_int buf pos 5 in
    let pos = Write.bin_write_char buf pos (char_of_int 4) in
    Write.bin_write_network32_int buf pos index
  | Bitfield bitfield -> 
    let len = Bitfield.length bitfield in
    let pos = Write.bin_write_network32_int buf pos (len + 1) in 
    let pos = Write.bin_write_char buf pos (char_of_int 5) in
    Common.blit_string_buf (Bitfield.to_string bitfield) ~dst_pos:pos buf ~len;
    pos + len
  | Request (index, begn, len) -> 
    let pos = Write.bin_write_network32_int buf pos 13 in
    let pos = Write.bin_write_char buf pos (char_of_int 6) in
    let pos = Write.bin_write_network32_int buf pos index in
    let pos = Write.bin_write_network32_int buf pos begn in
    Write.bin_write_network32_int buf pos len
  | Piece (index, begn, block) ->
    let len = String.length block in
    let pos = Write.bin_write_network32_int buf pos (len + 9) in
    let pos = Write.bin_write_char buf pos (char_of_int 7) in
    let pos = Write.bin_write_network32_int buf pos index in
    let pos = Write.bin_write_network32_int buf pos begn in
    Common.blit_string_buf block ~dst_pos:pos buf ~len;
    pos + len
  | Cancel (index, begn, len) ->
    let pos = Write.bin_write_network32_int buf pos 13 in
    let pos = Write.bin_write_char buf pos (char_of_int 8) in
    let pos = Write.bin_write_network32_int buf pos index in
    let pos = Write.bin_write_network32_int buf pos begn in
    Write.bin_write_network32_int buf pos len
  | Port port ->
    let pos = Write.bin_write_network32_int buf pos 3 in
    let pos = Write.bin_write_char buf pos (char_of_int 9) in
    Write.bin_write_network16_int buf pos port 
  | Extended (i, b) -> 
    let len = String.length b in
    let pos = Write.bin_write_network32_int buf pos (len + 1) in
    let pos = Write.bin_write_char buf pos (char_of_int 20) in
    let pos = Write.bin_write_char buf pos (char_of_int i) in
    Common.blit_string_buf b ~dst_pos:pos buf ~len;
    pos + len

let to_string m =
  match m with 
  | KeepAlive -> "KeepAlive"
  | Choke -> "Choke"
  | Unchoke -> "Unchoke"
  | Interested -> "Interested"
  | Not_interested -> "Not_interested"
  | Have i -> sprintf "Have i = %d" i
  | Bitfield s -> "Bitfield"
  | Request (i,b,l) -> sprintf "Request i = %d" i
  | Piece (i,off,_) -> sprintf "Piece i = %d, off = %d "  i off
  | Cancel _ -> "Cancel"
  | Port i -> sprintf "Port %d" i
  | Extended (i,b) -> "Extended"




