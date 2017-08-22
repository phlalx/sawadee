open Core
open Async
open Log.Global

(* TODO make this a ounit test *)

let test file port  =
  let%bind hash = 
    let f = In_channel.read_all file in
    Wrapper.add_torrent f port in
  let%bind () = Clock.after (sec 5.0) in 
  let%bind status = Wrapper.status hash port in 
  match status with
  | Some { torrent = Some { tinfo; downloaded }} ->  
    let num_dl = Bitfield.card downloaded in
    if not (num_dl = tinfo.Torrent.num_pieces) then
      failwith "didn't downloaded all the pieces";
    Wrapper.terminate port 
  | _ -> failwith "Status not available"

let process port num_clients file () : unit Deferred.t = 
  set_level `Info;
  List.range port (port + num_clients) 
  |> Deferred.List.iter ~f:(test file) ~how:`Parallel 
  >>| fun () -> Print.printf "Test OK\n"

let () = 
  let spec =
    Command.Spec.(
      empty +> 
      flag "-r" (required int) ~doc:"set base port"  +>  
      flag "-n" (required int) ~doc:"set num clients" +>  
      anon ("URI/FILE" %: string))
  in
  let command =
    Command.async ~summary:"Download torrent file" spec process
  in
  Command.run command
