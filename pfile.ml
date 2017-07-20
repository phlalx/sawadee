open Core
open Async
open Log.Global

type t = {
  name : string;
  fd : Unix.Fd.t;
  len : int;
  off : int;
}

let to_string t = sprintf "name = %s off = %d len %d" t.name t.off t.len

let create name ~len ~off = 
  let name = Global.path ^ name in
  info "create file %s" name;
  let llen = Int64.of_int len in
  let%bind fd = Unix.openfile name ~mode:[`Creat;`Rdwr] in
  Unix.ftruncate fd llen >>| fun () ->
  { name; fd; len; off }

let close t =
  info "close file %s" t.name;
  Unix.close t.fd

let seek_and_io t bytes ~ps operation =
  let offset = Int64.of_int t.off in
  let pos = t.off % ps in
  let len = t.len in
  let%bind off_res = Unix.lseek t.fd offset ~mode:`Set in
  assert (offset = off_res);
  let f desc = 
    match operation with 
    | `Read -> Bigstring.read ~pos ~len desc bytes
    | `Write -> Bigstring.write ~pos ~len desc bytes
  in
  info "read from %s off = %d len = %d" t.name t.off t.len;
  match Fd.syscall t.fd f with
  | `Already_closed -> assert false
  | `Ok l -> assert (l = t.len); Deferred.unit
  | `Error exn  -> raise exn
 
let write t bytes ~ps =
  seek_and_io t bytes ~ps `Write

let read t bytes ~ps =
  seek_and_io t bytes ~ps `Read
