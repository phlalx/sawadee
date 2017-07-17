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

let rec align_along_piece_size l ps =
 match l with 
  | [] -> []
  | {name; fd; len; off} :: t when (off % ps) + len <= ps -> 
    {name; fd; len; off} :: (align_along_piece_size t ps) 
  | {name; fd; len; off} :: t -> 
    let len' = ps - (off % ps) in
    assert (len' > 0);
    assert( ((off + len') % ps) = 0);
    { name; fd; len = len'; off } :: 
    (align_along_piece_size 
       ({ name; fd; off = off + len'; len = len - len'} :: t) 
       ps)

(* TODO: try to simplify/improve this *)
let rec split_along_piece_size l ~ps ~num_piece =
  let a = List.to_array (align_along_piece_size l ps) in
  let res = Array.create num_piece [] in 
  let j = ref 0 in
  let cur_len = ref 0 in
  let tl = ref [] in
  let m = Array.length a in
  for i = 0 to num_piece -1 do 
    tl := [];
    cur_len := 0;
    while !j < m && !cur_len < ps do 
      tl := !tl @ [ a.(!j) ];
      cur_len := !cur_len + a.(!j).len;
      incr j;
    done;
    res.(i) <- !tl;
  done;
  assert (!j = m);
  res 

let create name ~len ~off = 
  let name = Global.path ^ name in
  info "create file %s" name;
  let llen = Int64.of_int len in
  Unix.openfile name ~mode:[`Creat;`Rdwr]
  >>= fun fd ->
  Unix.ftruncate fd llen
  >>| fun () ->
  { name; fd; len; off }

let close t =
  info "close file %s" t.name;
  Unix.close t.fd

let debug = true 

let reader_read rd s ~pos ~len =
  if debug then
    return (String.fill s ~pos ~len '\000')
  else 
    Deferred.ignore (Reader.read rd s ~pos ~len)

let writer_write wr s ~pos ~len =
  if debug then
    ()
  else 
    Writer.write wr s ~pos ~len

let read t s ~ps = 
     info "read from %s off = %d len = %d" t.name t.off t.len;
     let offl = Int64.of_int t.off in
     Async_unix.Unix_syscalls.lseek t.fd ~mode:`Set offl
     >>= fun _ -> 
     let rd = Reader.create t.fd in
     let pos = t.off % ps in
     let len = t.len in
     reader_read rd s ~pos ~len

let write t s ~ps = 
     info "write to %s off = %d len = %d" t.name t.off t.len;
     let offl = Int64.of_int t.off in
     Async_unix.Unix_syscalls.lseek t.fd ~mode:`Set offl
     >>| fun _ -> 
     let wr = Writer.create t.fd in
     let pos = t.off % ps in
     let len = t.len in
     writer_write wr s ~pos ~len 

