open Core
open Async
open Log.Global

module G = Global
module Em = Error_msg

let set_verbose i =
  match i with
  | 1 -> set_level `Info; 
  | 2 -> set_level `Debug;
  | _ -> Em.terminate (Em.verbose_error ()) 

let check_path p = 
  match%bind Sys.is_directory p with 
  | `Yes -> return ()
  | `No | `Unknown -> Em.terminate (Em.can't_open p)

let set_server = function
  | None -> Deferred.unit
  | Some port ->
    G.set_port port; 
    Server.start ~port

let try_add_nis dht nis =
  let f (_, p) = Dht.try_add dht p |> Deferred.ignore in
  Deferred.List.iter ~how:`Parallel nis ~f

let set_dht = function
  | None -> Deferred.unit
  | Some port -> 
    let dht = Dht.create ~port (Node_id.random ()) in
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
  set_level `Error;
  Option.iter verbose ~f:set_verbose;
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
  set_dht dht_port

let add_torrent = Start.process_torrent 

let add_magnet s = 
  assert (G.is_dht ());
  Bt_hash.of_hex s |> Start.process_magnet 

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
    _ -> info "%s" (Em.can't_open table_name)

let terminate () =
  Torrent_table.data () |> Deferred.List.iter ~f:Pwp.close
  >>= fun () ->
  (* TODO we could terminate the server too *)
  Option.iter (G.dht ()) terminate_dht;
  flushed () 

let status h = 
  Torrent_table.find h |> Option.map ~f:Pwp.status 



