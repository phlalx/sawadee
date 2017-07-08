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
  | Have of int32 
  | Bitfield of string 
  | Request of int32 * int32  * int32
  | Piece of int32 * int32 * string
  | Cancel of int32 * int32 * int32
[@@deriving sexp]

(* Always 4 bytes for length prefix, 1 byte for type, and rest for payload *)
let size m = 
  match m with
  | KeepAlive -> 4
  | Choke -> 5
  | Unchoke -> 5
  | Interested -> 5
  | Not_interested -> 5
  | Have _ -> 9
  | Bitfield s -> 5 + (String.length s) 
  | Request _ -> 17
  | Piece (_,_,s) -> 13 + (String.length s)
  | Cancel _ -> 17

(* length is prefix-length, the number of bytes left to read, *)
let bin_read_payload buf ~pos_ref length =
  let x = Read.bin_read_char buf pos_ref in 
  match (int_of_char x) with
  | 0 -> Choke
  | 1 -> Unchoke
  | 2 -> Interested
  | 3 -> Not_interested
  | 4 -> let index = Read.bin_read_network32_int32 buf pos_ref in Have index
  | 5 -> 
    let bitfield_len = length - 1 in
    let bitfield = Bytes.create bitfield_len in
    Common.blit_buf_string ~src_pos:!pos_ref buf bitfield ~len:bitfield_len;
    Bitfield bitfield 
  | 6 -> 
    let index = Read.bin_read_network32_int32 buf pos_ref in
    let begn = Read.bin_read_network32_int32 buf pos_ref in
    let len = Read.bin_read_network32_int32 buf pos_ref in
    Request (index, begn, len)
  | 7 -> 
    let block_len = length - 9 in
    let index = Read.bin_read_network32_int32 buf pos_ref in
    let begn = Read.bin_read_network32_int32 buf pos_ref in
    let block = Bytes.create block_len in
    Common.blit_buf_string ~src_pos:!pos_ref buf block ~len:block_len; 
    Piece (index, begn, block)
  | 8 -> 
    let index = Read.bin_read_network32_int32 buf pos_ref in
    let begn = Read.bin_read_network32_int32 buf pos_ref in
    let len = Read.bin_read_network32_int32 buf pos_ref in
    Cancel (index, begn, len)
  | _ -> assert false

let bin_read_t buf ~pos_ref = 
  let length = Int32.to_int (Read.bin_read_network32_int32 buf pos_ref) in 
  match length with
  | Some(0) -> KeepAlive
  | Some(length) -> bin_read_payload buf ~pos_ref length
  | None -> assert(false)

let bin_write_t (buf:Common.buf) ~(pos:Common.pos) (x:t) =
  match x with
  | KeepAlive -> Write.bin_write_network32_int32 buf pos 0l
  | Choke -> 
    let pos = Write.bin_write_network32_int32 buf pos 1l in
    Write.bin_write_char buf pos (char_of_int 0) 
  | Unchoke ->
    let pos = Write.bin_write_network32_int32 buf pos 1l in
    Write.bin_write_char buf pos (char_of_int 1) 
  | Interested ->
    let pos = Write.bin_write_network32_int32 buf pos 1l in
    Write.bin_write_char buf pos (char_of_int 2) 
  | Not_interested ->
    let pos = Write.bin_write_network32_int32 buf pos 1l in
    Write.bin_write_char buf pos (char_of_int 3) 
  | Have index -> 
    let pos = Write.bin_write_network32_int32 buf pos 5l in
    let pos = Write.bin_write_char buf pos (char_of_int 4) in
    Write.bin_write_network32_int32 buf pos index
  | Bitfield bitfield -> 
    let len = String.length bitfield in
    let pos = Write.bin_write_network32_int buf pos (len + 1) in 
    let pos = Write.bin_write_char buf pos (char_of_int 5) in
    Common.blit_string_buf bitfield ~dst_pos:pos buf ~len;
    pos + len
  | Request (index, begn, len) -> 
    let pos = Write.bin_write_network32_int32 buf pos 13l in
    let pos = Write.bin_write_char buf pos (char_of_int 6) in
    let pos = Write.bin_write_network32_int32 buf pos index in
    let pos = Write.bin_write_network32_int32 buf pos begn in
    Write.bin_write_network32_int32 buf pos len
  | Piece (index, begn, block) ->
    let len = String.length block in
    let pos = Write.bin_write_network32_int buf pos (len + 9) in
    let pos = Write.bin_write_char buf pos (char_of_int 7) in
    let pos = Write.bin_write_network32_int32 buf pos index in
    let pos = Write.bin_write_network32_int32 buf pos begn in
    Common.blit_string_buf block ~dst_pos:pos buf ~len;
    pos + len
  | Cancel (index, begn, len) ->
    let pos = Write.bin_write_network32_int32 buf pos 13l in
    let pos = Write.bin_write_char buf pos (char_of_int 8) in
    let pos = Write.bin_write_network32_int32 buf pos index in
    let pos = Write.bin_write_network32_int32 buf pos begn in
    Write.bin_write_network32_int32 buf pos len

let to_string m =
  match m with 
  | KeepAlive -> "KeepAlive"
  | Choke -> "Choke"
  | Unchoke -> "Unchoke"
  | Interested -> "Interested"
  | Not_interested -> "Not_interested"
  | Have i -> "Have i = " ^ Int32.to_string i
  | Bitfield s -> "Bitfield"
  | Request (i,b,l) -> "Request i = " ^ Int32.to_string i
  | Piece (i,_,_) -> "Piece i = " ^ Int32.to_string i
  | Cancel _ -> "Cancel"