(**  Manages command-line options and calls [Start.process]. *)

open Core
open Async
open Log.Global

let terminate _ = 
  don't_wait_for (
    Bittorrent.terminate () 
    >>= fun () ->
    exit 0   (* TODO try to terminate without exit 0 *)
  )

let process 
    (path : string) 
    (port : int option) 
    (verbose : int option)
    (node : bool)
    (uri : string)
    ()
     : unit Deferred.t
  = 
  set_level `Error;

  (* we use same port for server and dht *)
  Bittorrent.create 
    ~download_path:path 
    ~torrent_path:path
    ~verbose
    ~server_port:port
    ~dht_port:port
  >>= fun () ->
  Signal.handle Signal.terminating ~f:terminate;

  let _handler = match Utils.parse_uri uri with 
    | `Magnet s -> Bittorrent.add_magnet s 
    | `File f -> In_channel.read_all f |> Bittorrent.add_torrent 
    | `Invalid_magnet | `Other -> assert false
  in

  never ()

let () = 
  let spec =
    Command.Spec.(
    empty +> 
    flag "-p" (required string) ~doc:" set download path" +> 
    flag "-l" (optional int) ~doc:" set server mode with port" +> 
    flag "-v" (optional int) ~doc:" verbose (level = 1 or 2)" +> 
    flag "-n" no_arg ~doc:" Enable DHT" +> 
    anon ("URI/FILE" %: string))
  in
  Command.async ~summary:"Download torrent file" spec process |> Command.run 

