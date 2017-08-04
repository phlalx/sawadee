open Core
open Async
open Log.Global

module G = Global
module P = Peer_comm
module Em = Error_msg

let ignore_error addr : unit Or_error.t -> unit =
  function 
  | Ok () -> () 
  | Error err -> 
    info !"Start: can't connect to %{Addr}" addr;
    debug !"Start: %{Error.to_string_hum}" err

let add_peers pwp info_hash addrs : unit Deferred.t =

  let add_peer addr = 
    let open Deferred.Or_error.Monad_infix in 
    P.create_with_connect addr
    >>= fun p ->
    P.initiate_handshake p info_hash 
    >>= fun { extension; dht } -> 
    let peer = Peer.create p ~extension ~dht in 
    Pwp.add_peer pwp peer 
    >>= fun () -> 
    P.close p |> Deferred.ok
  in

  let f addr = add_peer addr >>| ignore_error addr in
  Deferred.List.iter ~how:`Parallel addrs ~f

let parse_uri f =

  let uri = Uri.of_string f in

  (* st should be of the form "urn:btih:hex_info_hash" *)
  let decode_xt st = 
    match (String.length st) = 49 with
    | true ->
      let info_hash = String.sub st ~pos:9 ~len:40 |> Bt_hash.of_hex in
      `Magnet info_hash
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

let process_magnet hash = 
  info !"Start: processing magnet %{Bt_hash.to_hex}" hash;
  let pwp = Pwp.create () in
  let%bind addrs = Krpc.lookup hash in
  add_peers pwp hash addrs

let terminate nf _ =
  don't_wait_for (
    Network_file.close nf  
    >>= fun () ->
    Krpc.write_routing_table ();
    flushed () 
    >>= fun () -> 
    exit 0 
  )

let process_file f = 
  (***** read torrent file *****)
  let t = try 
      Torrent.from_file f
    with
    | Sys_error _ -> Em.terminate (Em.wrong_file f)
    | Failure s -> Em.terminate (Em.not_bencode f)
    | ex -> raise ex
  in 

  let open Torrent in
  let { torrent_name; info_hash; announce; announce_list; tinfo } = t in

  (***** get list of peers ****)

  let uris = 
    match announce_list with
    | [] -> [ announce ]
    | al -> List.dedup (List.concat al) |> List.permute 
  in

  let%bind addrs = Tracker_client.query info_hash uris in

  let num_of_peers = List.length addrs in 
  info "Start: %d tracker peers" num_of_peers;

  (******* create pwp and add peers ****)

  (* There are two types of peers. 
     - those that we contact  (got their addresses from the tracker
     - those that contact us (via the server) *)

  let bf_name = sprintf "%s/%s%s" (G.path ()) (Filename.basename torrent_name) 
      G.bitset_ext in

  let%bind nf = Network_file.create tinfo bf_name in

  Signal.handle Signal.terminating ~f:(terminate nf);

  let pwp = Pwp.create ~nf () in

  let peers = add_peers pwp info_hash addrs in 

  if G.is_server () then Server.add info_hash pwp; 

  (* TODO *)
  Deferred.all_unit [peers; never()]

let process uri =
  match parse_uri uri with
  | `File f -> process_file f 
  | `Other -> failwith "scheme error"
  | `Invalid_magnet -> failwith "invalid magnet"
  | `Magnet info_hash when G.is_node () -> process_magnet info_hash
  | `Magnet _ -> failwith "node must be enabled"

