open Core
open Async

let wrong_file f = Printf.sprintf "Can't access file %s" f

let not_bencode f = 
    Printf.sprintf "Can't decode file %s. Doesn't seem to be bencode." f

let wrong_bencode f = 
  Printf.sprintf "Can't decode file %s. Some keys are missing or don't have\
   expected content." f

let tracker_error () = "Can't connect to tracker" 

let tracker_error () = "Can't connect to tracker" 

let verbose_error () = "Verbose level should be 1 or 2" 

let can't_open p = Printf.sprintf "Can't open %s" p

(* This should be only called in Main or Start. Once we reached the end of 
   Start.process, no error should be fatal. *)
let terminate s =
  Core.Printf.printf "%s\n" s;
  Pervasives.flush_all ();
  Pervasives.exit 0
