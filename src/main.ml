(** Entry point to the project. 

    It implements the command-line options and call [Start.process] *)

open Core
open Async
open Log.Global
module G = Global

let process 
    (torrent_name : string)
    (port : int option) (* TODO ajouter le port dans la query *)
    (path : string option) = 

  (match path with
   | None -> ()
   | Some p -> G.set_path p);
  (match port with
   | None -> ()
   | Some p -> G.set_port p);

  don't_wait_for (Start.process torrent_name)

let spec =
  let open Command.Spec in
  empty +> 
  flag "-p" (optional string) ~doc:" set path (default = 'download')" +> 
  flag "-l" (optional int) ~doc:" set server mode with port" +> 
  flag "-v" (no_arg) ~doc:" verbose" +> 
  anon ("FILE" %: string) 

let command =
  Command.basic ~summary:"Download torrent file" spec
    (fun path port verbose filename () -> process filename port path)

let () = 
  set_level `Debug;
  Command.run command;
  never_returns (Scheduler.go ())



