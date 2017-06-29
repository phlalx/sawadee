open Core
open Async
open Log.Global

let some_or_fail x =
  match x with
  | Some y -> y
  | None -> failwith "bad torrent structure"

let bc_get_infos bc = 
  let announce_bc = some_or_fail (Bencode.dict_get bc "announce") in
  let announce_str = some_or_fail (Bencode.as_string announce_bc) in
  let info_bc = some_or_fail (Bencode.dict_get bc "info") in 
  let pieces_bc = some_or_fail (Bencode.dict_get info_bc "pieces") in
  let pieces_str = some_or_fail (Bencode.as_string pieces_bc) in
  let name_bc = some_or_fail (Bencode.dict_get info_bc "name") in
  let name_str = some_or_fail (Bencode.as_string name_bc) in 
  announce_str, pieces_str, name_str

let do_download f = 
  let s = In_channel.read_all f in 
  let bc = Bencode.decode (`String s) in 
  let s = Bencode.pretty_print bc in
  let announce, _, name_str = bc_get_infos bc in 
  string s;
  string announce;
  string name_str

let spec =
  let open Command.Spec in
  empty
  +> anon ("FILE" %: string)

let command =
  Command.basic
    ~summary:"Download torrent file"
    spec
    (fun filename -> (fun () -> (do_download filename)))

let main () = 
  Command.run command;
  flushed () >>= fun () ->
  exit 0

let () = 
  ignore (main ());
  never_returns (Scheduler.go ())

