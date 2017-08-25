open Core
open Async
open Blog

module G = Global
module Nf = Network_file 

let nf_of_tinfo info_hash tinfo seeder = 
  match tinfo with
  | None -> return None
  | Some tinfo -> let%map nf = Nf.create ~seeder info_hash tinfo in Some nf

let copy f1 f2 =
  let len = 65536 in
  let buffer = String.create len in
  let f rd : unit Deferred.t = 
    let g wr : unit Deferred.t =
      Deferred.repeat_until_finished ()
        (fun () -> 
           match%map Reader.read rd buffer ~pos:0 ~len with
           | `Eof -> `Finished ()
           | `Ok i -> Writer.write wr buffer ~pos:0 ~len:i; `Repeat ())
    in 
    Writer.with_file f2 ~f:g 
  in
  Reader.with_file f1 ~f 

let add_any info_hash 
    (tinfo : Torrent.info option)
    (uri : Uri.t option) 
    (seeder : bool) =
  info !"Bittorrent: add any %{Bt_hash.to_string_hum} tinfo = %b uri = %b seeder = %b"
    info_hash (Option.is_some tinfo) (Option.is_some uri) seeder;
  if not (Torrent_table.has_hash info_hash) then (
    let%map nf = nf_of_tinfo info_hash tinfo seeder in 
    let swarm = Swarm.create uri nf info_hash in  
    Torrent_table.add info_hash swarm; 
    Swarm.start swarm
  ) else ( 
    info !"Bittorrent: ignore, already in table";
    Deferred.unit )

let seed name ~piece_length = 
  info !"Bittorrent: seed %s" name;
  let f () =
    let tinfo = Torrent.info_of_file name piece_length in
    info !"Bittorrent: seeding %{Torrent.info_to_string_hum}" tinfo;
    let info_hash = Torrent.info_to_string tinfo |> Bt_hash.sha1_of_string in 
    let dst = G.with_download_path (Filename.basename name) in
    (copy name dst >>= fun () ->
     add_any info_hash (Some tinfo) None true) |> don't_wait_for;
    info_hash
  in Or_error.try_with f (* TODO change this *)

let add_magnet h = 
  if not (G.is_dht ()) then
    failwith "DHT should be enabled for magnets";
  let info_hash = Bt_hash.of_hex h in
  info !"Bittorrent: add magnet %{Bt_hash.to_string_hum}" info_hash;
  let n = h |> G.torrent_name |> G.with_torrent_path in
  let tinfo = Option.try_with
      (fun () -> In_channel.read_all n |> Torrent.info_of_string)  in

  add_any info_hash tinfo None false |> don't_wait_for;
  info_hash

let add_torrent s = 
  let t = try 
      Torrent.of_string s
    with
    | Failure _ -> failwith ("unable to decode" ^ s)
    | ex -> raise ex
  in 

  let Torrent.{ info_hash; announce; tinfo } = t in
  info !"Bittorrent: add torrent %{Torrent.to_string_hum}" t; 

  add_any info_hash (Some tinfo) (Some announce) false |> don't_wait_for;
  info_hash

let set_verbose i =
  match i with
  | 0 -> set_level `Error; 
  | 1 -> set_level `Info; 
  | 2 -> set_level `Debug;
  | _ -> failwith "verbose level should be 1 or 2"

let check_path p = 
  match%bind Sys.is_directory p with 
  | `Yes -> return ()
  | `No | `Unknown -> failwith ("can't open " ^ p)

let set_server = function
  | None -> Deferred.unit
  | Some port ->
    G.set_port port; 
    Server.start ~port

let try_add_nis dht nis =
  let f (_, p) = Dht.try_add dht p |> Deferred.ignore in
  Deferred.List.iter ~how:`Parallel nis ~f

let set_dht torrent_path verbose = function
  | None -> Deferred.unit
  | Some port -> 
    let dht = Dht.create ~port (Node_id.random ()) ~data_path:torrent_path
      ~verbose in
    G.set_dht dht;
    let table_name = G.with_torrent_path G.routing_table_name in
    info "Bittorrent: trying to read dht table %s" table_name;
    let table = 
      try
        In_channel.read_all table_name |> Node_info.list_of_string_exn 
      with err ->  
        info "Bittorrent: can't read %s. Using empty table" table_name;
        debug !"Bittorrent: error processing %s. %{Exn}" table_name err;
        [] 
    in
    try_add_nis dht table
    >>| fun () -> 
    info "Bittorrent: added %d nodes to DHT" (List.length (Dht.table dht))

let create ~server_port ~verbose ~torrent_path ~download_path ~dht_port = 
  set_verbose verbose;
  info !"Bittorrent: peer-id:%{Peer_id.to_string_hum}" G.peer_id;
  G.set_download_path download_path;
  G.set_torrent_path torrent_path;
  check_path download_path 
  >>= fun () -> 
  check_path torrent_path 
  >>= fun () ->  
  let f = Log.Output.file `Text (G.with_torrent_path G.log_name) in 
  set_output [f; (Log.Output.stderr ())];
  set_server server_port
  >>= fun () ->  
  set_dht torrent_path verbose dht_port

let torrent_list () = 
  Torrent_table.keys ()

let terminate_dht dht =
  let table_name = G.with_torrent_path G.routing_table_name in
  info "Bittorrent: trying to write dht table %s" table_name;
  try 
    let data = Dht.table dht |> Node_info.list_to_string in 
    Out_channel.write_all table_name ~data;
    info "Bittorrent: writing %s" table_name;
  with
    _ -> info "Bittorrent: can't open %s" table_name

let terminate () =
  Torrent_table.data () |> Deferred.List.iter ~f:Swarm.close
  >>= fun () ->
  (* TODO should terminate the servers too *)
  Option.iter (G.dht ()) terminate_dht;
  flushed () 

let status h = 
  Torrent_table.find h |> Option.map ~f:Swarm.status 



