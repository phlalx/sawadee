open Core

let wrong_file f = Printf.sprintf "Can't access file %s" f

let not_bencode f = 
  Printf.sprintf "Can't decode file %s. Doesn't seem to be bencode." f

let wrong_bencode f = 
  sprintf "Can't decode file %s. Some keys are missing or don't have expected content." 
  f