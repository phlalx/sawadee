
(* http://bittorrent.org/beps/bep_0003.html *)

open Core

module Be = Bencode_ext

type ok = { interval : int; peers : Addr.t list }

type t = (ok, string) Result.t

let to_string r =
  (match r with
   | Ok r -> 
     Be.Dict [
       ("interval", Be.Integer r.interval);
       ("peers", Addr.list_to_bencode r.peers)
     ]
   | Error s -> Be.Dict [ ("failure reason", Be.String s)]) 
  |> Be.encode_to_string

let of_string s =
  let bc = `String s |> Be.decode in 
  match Be.dict_get bc "failure reason" with
  | None -> 
    let interval = Be.dict_get_int_exn bc "interval" in 
    let peers_bencode = Be.dict_get_exn bc "peers" in
    let peers = Addr.list_of_bencode peers_bencode in
    Ok { interval; peers }
  | Some s -> 
    Error (Be.as_string_exn s)


