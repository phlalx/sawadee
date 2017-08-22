open Core
open Async
open Log.Global

let piece_length = 65536 

let setup_seeder f port : string Deferred.t =
  match%map Wrapper.seed f port with
  | Ok hash -> Bt_hash.to_hex hash
  | Error err -> Print.printf !"%{sexp:Error.t}\n" err; failwith "can't seed"

let test_leecher uri port : unit Deferred.t =
  let%bind handler = Wrapper.add_magnet uri port in
  let%bind () = Clock.after (sec 5.0) in 
  let%bind status = Wrapper.status handler port in 
  match status with
  | Some { torrent = Some { tinfo; downloaded }} -> 
    let num_dl = Bitfield.card downloaded in
    if not (num_dl = tinfo.Torrent.num_pieces) then
      failwith "didn't downloaded all the pieces"; 
    Wrapper.terminate port 
  | _ -> failwith "Status not available"

let process port num_clients file () : unit Deferred.t = 
  set_level `Info;
  let l = List.range port (port + num_clients) in 
  match l with
  | hd :: tl -> 
    let%bind hash = setup_seeder (file, piece_length) port in
    let%map res = Deferred.List.iter tl ~f:(test_leecher hash) ~how:`Parallel in
    Print.printf "Test OK\n"
  | _ -> failwith "run test with at least two clients"


let () = 
  let spec =
    Command.Spec.(
      empty +> 
      flag "-r" (required int) ~doc:"set base port"  +>  
      flag "-n" (required int) ~doc:"set num clients" +>  
      anon ("FILE" %: string))
  in
  let command =
    Command.async ~summary:"Download torrent file" spec process
  in
  Command.run command
