(**  Manages command-line options and calls [Start.process]. *)

open Core
open Async

let terminate _ = 
  don't_wait_for (
    Bittorrent.terminate () 
    >>= fun () ->
    exit 0 
  )

let process 
    (path : string) 
    (port : int) 
    (verbose : int option)
    (uri : string)
    ()
     : unit Deferred.t
  = 

  (* we use same port for server and dht *)
  Bittorrent.create 
    ~download_path:path 
    ~torrent_path:path
    ~verbose:(Option.value ~default:0 verbose)
    ~server_port:(Some port)
    ~dht_port:(Some port)
  >>= fun () ->
  Signal.handle Signal.terminating ~f:terminate;

  let _handler = match Utils.parse_uri uri with 
    | `Magnet s -> Bittorrent.add_magnet s 
    | `File f -> In_channel.read_all f |> Bittorrent.add_torrent 
    | `Invalid_magnet | `Other -> assert false
  in

  never ()

let () = 
  (* TODO see how to use let syntax for commands *)
  let spec =
    Command.Spec.(
    empty +> 
    flag "-p" (required string) ~doc:" set download path" +> 
    flag "-l" (required int) ~doc:" set port" +> 
    flag "-v" (optional int) ~doc:" verbose (level = 1 or 2)" +> 
    anon ("URI/FILE" %: string))
  in
  Command.async ~summary:"Download torrent/magnet." spec process |> Command.run 

