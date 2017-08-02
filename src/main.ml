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
    (uri : string)
    (port : int option) 
    (path : string) 
    (verbose : int option)
    (node : bool)
  = 
  set_level `Error;

  G.set_node node;
  Option.value_map port ~default:() ~f:G.set_port;
  G.set_path path;
  Option.value_map verbose ~default:() ~f:set_verbose;
  info !"This peer-id:%{Peer_id.to_readable_string}" G.peer_id;
  info !"This node-id:%{Node_id.to_readable_string}" G.node_id;
  check_path ()
  >>= fun () ->

  (if G.is_node () then 
    Krpc.read_routing_table () 
    >>= fun () ->
    Krpc.populate ()
  else
   Deferred.unit) 
  >>= fun () ->
  info "processed routing table";

  if G.is_server () then 
    Server.start ~port:(G.port_exn ())
  else 
    Deferred.unit;

  >>= fun () ->

  Start.process uri


let () = 
  let spec =
    let open Command.Spec in
    empty +> 
    flag "-p" (required string) ~doc:" set download path" +> 
    flag "-l" (optional int) ~doc:" set server mode with port" +> 
    flag "-v" (optional int) ~doc:" verbose (level = 1 or 2)" +> 
    flag "-n" no_arg ~doc:" Enable DHT" +> 
    anon ("URI/FILE" %: string) 
  in
  let command =
    Command.async ~summary:"Download torrent file" spec
      (fun path port verbose node filename () -> 
         process filename port path verbose node)
  in
  Command.run command

