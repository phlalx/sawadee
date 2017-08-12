
(* http://bittorrent.org/beps/bep_0003.html *)

open Core

module B = Bencode_ext

type ok = { interval : int; peers : Addr.t list }

type t = (ok, string) Result.t

let to_string r =
  (match r with
   | Ok r -> 
     B.Dict [
       ("interval", B.Integer r.interval);
       ("peers", Addr.list_to_bencode r.peers)
     ]
   | Error s -> B.Dict [ ("failure reason", B.String s)]) 
  |> B.encode_to_string

let of_string s =
  let bc = `String s |> B.decode in 
  match B.dict_get bc "failure reason" with
  | None -> 
    let interval = B.dict_get_int_exn bc "interval" in 
    let peers_bencode = B.dict_get_exn bc "peers" in
    let peers = Addr.list_of_bencode peers_bencode in
    Ok { interval; peers }
  | Some s -> 
    Error (B.as_string_exn s)


