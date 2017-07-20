open Core
open Async
open Log.Global

let seek_and_io fd bytes ~off ~pos ~len operation =
  let offset = Int64.of_int off in
  let%bind off_res = Unix.lseek fd offset ~mode:`Set in
  assert (offset = off_res);
  let f desc = 
    match operation with 
    | `Read -> Bigstring.read ~pos ~len desc bytes
    | `Write -> Bigstring.write ~pos ~len desc bytes
  in
  info "read/write from off = %d len = %d" off len;
  match Fd.syscall fd f with
  | `Already_closed -> assert false
  | `Ok l -> assert (l = len); Deferred.unit
  | `Error exn  -> raise exn
 
let write fd bytes ~off ~pos ~len =
  seek_and_io fd bytes ~off ~pos ~len `Write

let read fd bytes ~off ~pos ~len =
  seek_and_io fd bytes ~off ~pos ~len `Read







