open Core
open Async

type t = {
  name : string;
  fd : Unix.Fd.t;
  offset : int64; (* offset in the "big file" *)
  len : int64;
}

val create : 'a

val read_to_pieces : 'a

val write_from_pieces : 'a

val close : 'a

val read_to_string : 'a

val write_to_string : 'a

(*
 Unix.openfile name ~mode:[`Creat;`Rdwr]  
  >>= fun file_fd -> (* TODO check these values *)
  Unix.ftruncate file_fd (Int64.of_int len)
  >>= fun _ -> 
  Unix.openfile (bitset_name name) ~mode:[`Creat;`Rdwr]
  >>= fun bitset_fd ->
  Unix.ftruncate bitset_fd (Int64.of_int (Bitset.bitfield_length owned_pieces))
  >>= fun () ->
  info "create file (num piece = %d, name = %s)" num_pieces name;
  let t = { len; name; num_pieces; pieces; info_hash = hash; owned_pieces; 
            file_fd; bitset_fd; piece_length } 
  in 
  read t 
  >>| fun () ->
  t
  *)