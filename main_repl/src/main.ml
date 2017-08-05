(**  Manages command-line options and calls [Start.process]. *)

open Core
open Async
open Log.Global

let terminate () = 
  Bittorrent.terminate () 
  >>= fun () ->
  exit 0  

let prompt = "# "

let parse s =
  String.split_on_chars ~on:[' '; '\t'] s 
  |> List.filter ~f:(fun s -> String.is_empty s |> not)  

exception Invalid

let eval_add s = 
  try 
   let handler = match Bittorrent.parse_uri s with 
     | `Magnet s -> Bittorrent.add_magnet s 
     | `File f -> In_channel.read_all f |> Bittorrent.add_torrent 
     | `Invalid_magnet | `Other -> raise Invalid
   in
   Print.printf !"Added %{Bittorrent.handler_to_string}.\n" handler
 with 
  | Invalid -> Print.printf "Can't add %s: not a magnet.\n" s
  | Sys_error _ -> Print.printf "Can't open file %s.\n" s

let eval_list () =
  let f = Print.printf !"%{Bittorrent.handler_to_string}\n" in
  Bittorrent.torrent_list () |> List.iter ~f

let help () = 
  Print.printf "Commands are: \n";
  Print.printf " add\n";
  Print.printf " list\n";
  Print.printf " status \n";
  Print.printf " quit\n";
  Print.printf " help\n"

let status_to_string (st:Bittorrent.status) =
  let open Bittorrent in 
  sprintf "num_peers = %d\n" st.num_peers 

let eval_status t : unit Option.t = 
  let open Option.Monad_infix in
  Bittorrent.handler_of_string t 
  >>= 
  Bittorrent.status 
  >>= fun (st:Bittorrent.status) -> 
  Print.printf !"%{status_to_string}" st; 
  Some ()

let eval = function 
  | "add" :: s :: [] -> eval_add s |> return
  | "list" :: [] -> eval_list () |> return
  | "status" :: h :: [] -> (
    match eval_status h with
    | None -> Print.printf "%s is not a proper handler\n" h
    | Some _ -> ()) |> return
  | "quit" :: [] -> terminate ()
  | "help" :: [] -> help () |> return
  | _ -> Print.printf "bad command\n" |> return

let rec repl stdin =
  Print.printf "%s" prompt;
  match%bind Reader.read_line stdin with
  | `Ok l -> parse l |> eval >>= fun () -> repl stdin 
  | `Eof -> terminate ()

let process 
    (port : int option) 
    (path : string) 
    (verbose : int option)
    (node : bool) : unit Deferred.t
  = 
  set_level `Error;

  Bittorrent.create 
    ~download_path:path 
    ~torrent_path:path
    ~verbose:None
    ~server_port:port
  >>= fun () ->

  Signal.handle Signal.terminating ~f:(fun _ -> terminate () |> don't_wait_for);

  let stdin = Lazy.force Reader.stdin in 
  repl stdin

let () = 
  let spec =
    let open Command.Spec in
    empty +> 
    flag "-p" (required string) ~doc:" set download path" +> 
    flag "-l" (optional int) ~doc:" set server mode with port" +> 
    flag "-v" (optional int) ~doc:" verbose (level = 1 or 2)" +> 
    flag "-n" no_arg ~doc:" Enable DHT"  
  in
  let command =
    Command.async ~summary:"Download torrent file" spec
      (fun path port verbose node () -> 
         process port path verbose node)
  in
  Command.run command

