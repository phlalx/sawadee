open Core
open Async
open Log.Global

module G = Global

let add_any info_hash 
    (tinfo : Torrent.info option)
    (uris : Uri.t list option) 
  : Bt_hash.t =
  let pwp = Pwp.create uris tinfo info_hash in  
  Torrent_table.add info_hash pwp; 
  Pwp.start pwp;
  info_hash

let seed name ~piece_length = 
  let f () =
    let tinfo = Torrent.info_of_file name piece_length in
    let info_hash = Torrent.info_to_string tinfo |> Bt_hash.sha1_of_string in 
    add_any info_hash (Some tinfo) None 
  in Or_error.try_with f

let add_magnet h = 
  if not (G.is_dht ()) then
    failwith "DHT should be enabled for magnets";
  let hash = Bt_hash.of_hex h in
  let n = h |> G.torrent_name |> G.with_torrent_path in
  let tinfo = Option.try_with
      (fun () -> In_channel.read_all n |> Torrent.info_of_string)  in

  add_any hash tinfo None

let add_torrent s = 
  let t = try 
      Torrent.of_string s
    with
    | Failure _ -> failwith ("unable to decode" ^ s)
    | ex -> raise ex
  in 

  let open Torrent in
  let { info_hash; announce; announce_list; tinfo } = t in

  let uris = 
    match announce_list with
    | [] -> [ announce ]
    | al -> List.dedup_and_sort (List.concat al) |> List.permute 
  in
  add_any info_hash (Some tinfo) (Some uris)

let set_verbose i =
  match i with
  | 0 -> ()
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
  Torrent_table.data () |> Deferred.List.iter ~f:Pwp.close
  >>= fun () ->
  (* TODO we could terminate the server too *)
  Option.iter (G.dht ()) terminate_dht;
  flushed () 

let status h = 
  Torrent_table.find h |> Option.map ~f:Pwp.status 



