open Core
open Async
open Log.Global

let piece_length = 65536 

let setup_seeder f port : string Deferred.t =
  match%map Wrapper.seed f port with
  | Ok hash -> Bt_hash.to_hex hash
  | Error err -> Print.printf !"%{sexp:Error.t}\n" err; failwith "can't seed"

let test_peer hash time port : bool Deferred.t =
  let%bind handler = Wrapper.add_magnet hash port in
  let%bind () = Clock.after (sec (float_of_int time)) in 
  let%bind status = Wrapper.status handler port in 
  match status with
  | Some { torrent = Some { tinfo; downloaded }} -> 
    let num_dl = Bitfield.card downloaded in
    let num_total = tinfo.Torrent.num_pieces in
    Print.printf "Peer %d: %d/%d\n" port  num_dl num_total;
    Wrapper.terminate port 
    >>| fun () ->
    num_dl = num_total
  | _ -> 
    Print.printf "Peer %d: didn't get status\n" port;
    Wrapper.terminate port 
    >>| fun () -> 
    false


let process port num_clients time file () : unit Deferred.t = 
  set_level `Info;
  let l = List.range port (port + num_clients) in 
  match l with
  | hd :: _ -> 
    let%bind hash = setup_seeder (file, piece_length) port in
    let%bind res = Deferred.List.map l ~f:(test_peer hash time) ~how:`Parallel in (
      match List.fold res ~init:true ~f:(fun acc b -> acc && b) with
      | true -> Print.printf "Test OK\n"; exit 0
      | false -> Print.printf "Test Not OK\n"; exit 1)
  | _ -> failwith "run test with at least two clients"


let () = 
  let spec =
    Command.Spec.(
      empty +> 
      flag "-r" (required int) ~doc:"set base port"  +>  
      flag "-n" (required int) ~doc:"set num clients" +>  
      flag "-s" (required int) ~doc:"time for test to complete" +>  
      anon ("FILE" %: string))
  in
  Command.async ~summary:"Download torrent file" spec process
  |> Command.run 
