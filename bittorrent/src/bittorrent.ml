open Core
open Async
open Log.Global

module G = Global
module Em = Error_msg

type handler = Bt_hash.t

let set_verbose i =
  match i with
  | 1 -> set_level `Info; 
  | 2 -> set_level `Debug;
  | _ -> Em.terminate (Em.verbose_error ()) 

let check_path p = 
  match%bind Sys.is_directory p with 
  | `Yes -> return ()
  | `No | `Unknown -> Em.terminate (Em.can't_open p)

let create ~server_port ~verbose ~torrent_path ~download_path  = 
  set_level `Error;

  info !"Bittorrent: peer-id:%{Peer_id.to_string_hum}" G.peer_id;
  info !"Bittorrent: node-id:%{Node_id.to_string_hum}" G.node_id;

  Option.value_map verbose ~default:() ~f:set_verbose;
  G.set_download_path download_path;
  G.set_torrent_path torrent_path;
  check_path download_path 
  >>= fun () -> 
  check_path torrent_path 
  >>= fun () -> 
  if G.is_server () then 
    Server.start ~port:(G.port_exn ())
  else 
    Deferred.unit

let add_torrent s = 
    Start.process_string s |> Deferred.ok

let add_magnet s = 
    Bt_hash.of_string s |> Start.process_magnet |> Deferred.ok

let torrent_list () = 
  Torrent_table.keys ()

let handler_to_string = Bt_hash.to_hex

let terminate () =
  Torrent_table.data () |> Deferred.List.iter ~f:Pwp.close
  >>= fun () ->
  Krpc.write_routing_table ();
  flushed () 

let parse_uri f =

  let uri = Uri.of_string f in

  (* st should be of the form "urn:btih:hex_info_hash" *)
  let decode_xt st = 
    match (String.length st) = 49 with
    | true ->
      let info_hash = String.sub st ~pos:9 ~len:40 in `Magnet info_hash
    | false -> `Invalid_magnet
  in

  let extract_param uri = 
    match Uri.get_query_param uri "xt" with
    | Some xt -> decode_xt xt 
    | None -> `Invalid_magnet
  in

  match Uri.scheme uri with
  | Some "magnet" -> extract_param uri 
  | Some "file" -> `File (Uri.path uri)
  | None -> `File f
  | _ -> `Other
