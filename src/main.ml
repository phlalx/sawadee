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
    | `No | `Unknown -> failwith (Em.can't_open p)

let process 
    (torrent_name : string)
    (port : int option) (* TODO ajouter le port dans la query *)
    (path : string option) = 

  info "This peer-id:%s" (Peer_id.to_readable_string G.peer_id);

  Option.value_map port ~default:() ~f:G.set_port;
  Option.value_map path ~default:() ~f:G.set_path;

  check_path () 
  >>= fun () ->

  Start.process torrent_name

let spec =
  let open Command.Spec in
  empty +> 
  flag "-p" (optional string) ~doc:" set path (default = 'download')" +> 
  flag "-l" (optional int) ~doc:" set server mode with port" +> 
  flag "-v" (no_arg) ~doc:" verbose" +> 
  anon ("FILE" %: string) 

let command =
  Command.basic ~summary:"Download torrent file" spec
    (fun path port verbose filename () -> 
      don't_wait_for (process filename port path))

let () = 
  set_level `Info;
  Command.run command;
  never_returns (Scheduler.go ())



