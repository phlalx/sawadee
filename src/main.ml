(** Entry point to the project. 

    It implements the command-line options and call [Start.process] *)

open Core
open Async
open Log.Global
module G = Global
module Em = Error_msg

let check_path () = 
  let p = G.path () in
  match%bind Sys.is_directory p with 
  | `Yes -> return ()
  | `No | `Unknown -> Em.terminate (Em.can't_open p)

let set_verbose i =
  match i with
  | 1 -> set_level `Info; 
  | 2 -> set_level `Debug;
  | _ -> assert false

let process 
    (torrent_name : string)
    (port : int option) 
    (path : string) 
    (verbose : int option)
    = 

  info "This peer-id:%s" (Peer_id.to_readable_string G.peer_id);

  Option.value_map port ~default:() ~f:G.set_port;
  G.set_path path;
  Option.value_map verbose ~default:() ~f:set_verbose;

  check_path () 
  >>= fun () ->
  Start.process torrent_name


let spec =
  let open Command.Spec in
  empty +> 
  flag "-p" (required string) ~doc:" set download path" +> 
  flag "-l" (optional int) ~doc:" set server mode with port" +> 
  flag "-v" (optional int) ~doc:" verbose (level = 1 or 2)" +> 
  anon ("FILE" %: string) 

let command =
  Command.basic ~summary:"Download torrent file" spec
    (fun path port verbose filename () -> 
       don't_wait_for (process filename port path verbose))

let () = 
  set_level `Error;
  Command.run command;
  never_returns (Scheduler.go ())



