(**  Manages command-line options and calls [Start.process]. *)

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
  | _ -> Em.terminate (Em.verbose_error ()) 

let process 
    (torrent_name : string)
    (port : int option) 
    (path : string) 
    (verbose : int option)
  = 
  set_level `Error;


  Option.value_map port ~default:() ~f:G.set_port;
  G.set_path path;
  Option.value_map verbose ~default:() ~f:set_verbose;
  info "This peer-id:%s" (Peer_id.to_readable_string G.peer_id);
  check_path ()
  >>= fun () ->
  Start.process torrent_name

let () = 
  let spec =
    let open Command.Spec in
    empty +> 
    flag "-p" (required string) ~doc:" set download path" +> 
    flag "-l" (optional int) ~doc:" set server mode with port" +> 
    flag "-v" (optional int) ~doc:" verbose (level = 1 or 2)" +> 
    anon ("FILE" %: string) 
  in
  let command =
    Command.async ~summary:"Download torrent file" spec
      (fun path port verbose filename () -> 
         process filename port path verbose)
  in
  Command.run command

