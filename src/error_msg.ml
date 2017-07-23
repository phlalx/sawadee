open Core

let wrong_file f = sprintf "Can't access file %s" f

let not_bencode f = 
    sprintf "Can't decode file %s. Doesn't seem to be bencode." f

let wrong_bencode f = 
  sprintf "Can't decode file %s. Some keys are missing or don't have expected content." 
  f

let tracker_error () = "Can't connect to tracker" 

let tracker_error () = "Can't connect to tracker" 

let can't_open p = sprintf "Can't open %s" p