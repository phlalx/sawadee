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
    let handler = match Utils.parse_uri s with 
      | `Magnet s -> Bittorrent.add_magnet s 
      | `File f -> In_channel.read_all f |> Bittorrent.add_torrent 
      | `Invalid_magnet | `Other -> raise Invalid
    in
    Print.printf !"Added %{Bt_hash.to_hex}.\n" handler
  with 
  | Invalid -> Print.printf "Can't add %s: not a magnet.\n" s
  | Sys_error _ -> Print.printf "Can't open file %s.\n" s

let eval_list () =
  let f = Print.printf !"%{Bt_hash.to_hex}\n" in
  Bittorrent.torrent_list () |> List.iter ~f

let help () = 
  Print.printf "Commands are: \n";
  Print.printf " add\n";
  Print.printf " list\n";
  Print.printf " status \n";
  Print.printf " quit\n";
  Print.printf " help\n"

let status_to_string st =
  let open Status in 
  sprintf "num_peers = %d\n" st.num_peers 

let eval_status t : unit =
  match Option.try_with (fun () -> Bt_hash.of_hex t) with 
  | None -> Print.printf "Bad handler. Not a hex hash.\n"
  | Some t -> 
    match Bittorrent.status t with
    | Some st -> Print.printf !"%{status_to_string}" st
    | None -> Print.printf "Not in table\n."

let eval = function 
  | "add" :: s :: [] -> eval_add s |> return
  | "list" :: [] -> eval_list () |> return
  | "status" :: h :: [] -> eval_status h |> return
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

