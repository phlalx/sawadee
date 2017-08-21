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

let eval_seed f ps =
  let piece_length = int_of_string ps in
  match Bittorrent.seed f ~piece_length with
  | Ok h -> 
    Print.printf !"Seeding %{Bt_hash.to_hex}.\n" h
  | Error err -> 
    Print.printf "Can't seed %s\n" f;
    Print.printf !"%{sexp:Error.t}\n" err

let help () = 
  Print.printf "Commands are: \n";
  Print.printf " add\n";
  Print.printf " seed\n";
  Print.printf " list\n";
  Print.printf " status \n";
  Print.printf " quit\n";
  Print.printf " help\n"

let eval_status t : unit =
  match Option.try_with (fun () -> Bt_hash.of_hex t) with 
  | None -> Print.printf "Bad handler. Not a hex hash.\n"
  | Some t -> 
    match Bittorrent.status t with
    | Some st -> Print.printf !"%{Status}" st
    | None -> Print.printf "Not in table\n."

let eval = function 
  | "add" :: s :: [] -> eval_add s |> return
  | "list" :: [] -> eval_list () |> return
  | "status" :: h :: [] -> eval_status h |> return
  | "seed" :: f :: ps :: [] -> eval_seed f ps |> return
  | "quit" :: [] -> terminate ()
  | "help" :: [] -> help () |> return
  | _ -> Print.printf "bad command\n" |> return

let rec repl stdin =
  Print.printf "%s" prompt;
  match%bind Reader.read_line stdin with
  | `Ok l -> parse l |> eval >>= fun () -> repl stdin 
  | `Eof -> terminate ()

let process (path : string) (port : int option)  (verbose : int option) 
    (uris : string list) () : unit Deferred.t
  = 
  set_level `Error;

  Bittorrent.create 
    ~download_path:path 
    ~torrent_path:path
    ~verbose:None
    ~server_port:port
    ~dht_port:port
  >>= fun () ->

  Signal.handle Signal.terminating ~f:(fun _ -> terminate () |> don't_wait_for);

  List.iter uris ~f:eval_add; 

  let stdin = Lazy.force Reader.stdin in 
  repl stdin

let () = 
  let spec =
    Command.Spec.(
      empty +> 
      flag "-p" (required string) ~doc:" set download path" +> 
      flag "-l" (optional int) ~doc:" set server mode with port" +> 
      flag "-v" (optional int) ~doc:" verbose (level = 1 or 2)" +> 
      anon (sequence ("URI/FILEs" %: string)))
  in
  Command.async ~summary:"Download torrent file" spec process |> Command.run

