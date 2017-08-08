open Core
open Async
open Log.Global

module G = Global
module Em = Error_msg

let init_krpc () =
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
  if G.is_dht () then Krpc.try_add_nis table else Deferred.unit

let terminate_dht () =
  let table_name = G.with_torrent_path G.routing_table_name in
  info "Bittorrent: trying to read dht table %s" table_name;
  try 
    let data = Krpc.table () |> Node_info.list_to_string in 
    Out_channel.write_all table_name ~data;
    info "Bittorrent: writing %s" table_name;
  with
    _ -> info "%s" (Em.can't_open table_name)

let set_verbose i =
  match i with
  | 1 -> set_level `Info; 
  | 2 -> set_level `Debug;
  | _ -> Em.terminate (Em.verbose_error ()) 

let check_path p = 
  match%bind Sys.is_directory p with 
  | `Yes -> return ()
  | `No | `Unknown -> Em.terminate (Em.can't_open p)

let create ~server_port ~verbose ~torrent_path ~download_path ~dht_port = 
  set_level `Error;
  Option.value_map dht_port ~default:() ~f:G.set_dht_port;
  Option.value_map server_port ~default:() ~f:G.set_port;
  Option.value_map verbose ~default:() ~f:set_verbose;
  G.set_download_path download_path;
  G.set_torrent_path torrent_path;
  check_path download_path 
  >>= fun () -> 
  check_path torrent_path 
  >>= fun () -> ( 
    let f = Log.Output.file `Text (G.with_torrent_path G.log_name) in 
    set_output [f; (Log.Output.stderr ())];
    if G.is_server () then 
      Server.start ~port:(G.port_exn ()) 
    else 
      Deferred.unit )
  >>= fun () ->  (
    if G.is_dht () then 
      init_krpc () 
    else 
      Deferred.unit)

let add_torrent s = Start.process_string s 

let add_magnet s = 
  assert (G.is_dht ());
  Bt_hash.of_hex s |> Start.process_magnet 

let torrent_list () = 
  Torrent_table.keys ()

let terminate () =
  Torrent_table.data () |> Deferred.List.iter ~f:Pwp.close
  >>= fun () ->
  if G.is_dht () then terminate_dht ();
  flushed () 

let status h = 
  let f pwp = Pwp.status pwp in 
  Torrent_table.find h |> Option.map ~f 



