(**  Manages command-line options and calls [Start.process]. *)

open Core
open Async
open Log.Global


let bye () =
  don't_wait_for (after (sec 1.0) >>= fun () -> exit 0)

let terminate () = 
  Bittorrent.terminate ()
  >>| bye

(* The list of RPC implementations supported by this server *)
let implementations =
  [ Rpc.Rpc.implement' Protocol.add_torrent_rpc (fun () -> Bittorrent.add_torrent);
    Rpc.Rpc.implement' Protocol.add_magnet_rpc (fun () -> Bittorrent.add_magnet);
    Rpc.Rpc.implement Protocol.terminate_rpc (fun () -> terminate);
    Rpc.Rpc.implement' Protocol.status_rpc (fun () -> Bittorrent.status); ]

  let process 
      (port : int option) 
      (rpc_port : int) 
      (path : string) 
      (verbose : int option)
      (node : bool) : unit Deferred.t
    = 
    set_level `Error;

    Bittorrent.create 
      ~download_path:path 
      ~torrent_path:path
      ~verbose:verbose
      ~server_port:port
      ~dht_port:None
    >>= fun () ->

    Signal.handle Signal.terminating ~f:(fun _ -> terminate () |> don't_wait_for);
    Common.start_server ~env:() ~port:rpc_port ~implementations ()

let () = 
  let spec =
    let open Command.Spec in
    empty +> 
    flag "-p" (required string) ~doc:" set download path" +> 
    flag "-l" (optional int) ~doc:" set server mode with port" +> 
    flag "-r" (required int) ~doc:" set RPC port" +> 
    flag "-v" (optional int) ~doc:" verbose (level = 1 or 2)" +> 
    flag "-n" no_arg ~doc:" Enable DHT"  
  in
  let command =
    Command.async ~summary:"Download torrent file" spec
      (fun path port rpc_port verbose node () -> 
         process port rpc_port path verbose node)
  in
  Command.run command

