open Core
open Async
open Log.Global

module G = Global
module P = Peer
module Em = Error_msg

let ignore_error addr : unit Or_error.t -> unit =
  function 
  | Ok () -> () 
  | Error err -> 
    info "Error connecting with peer %s" (Socket.Address.Inet.to_string addr);
    debug "Error connecting %s" (Sexp.to_string (Error.sexp_of_t err))

let add_connected_peer pwp kind info_hash num_pieces addr r w  =
  let open Deferred.Or_error.Monad_infix in 
  let peer = P.create addr r w kind in
  P.handshake peer info_hash G.peer_id
  >>= fun () ->
  Print.printf "handshake with (server) peer %s\n" (P.addr_to_string peer);
  P.init_size_owned_pieces peer num_pieces;
  Pwp.add_peer pwp peer

(* TODO must be a more elegant way of combining these monads *)
let add_connected_peer_and_close pwp kind info_hash num_pieces addr r w =
  let close x = Writer.close w >>= fun () -> Reader.close r >>| fun () -> x in
  add_connected_peer pwp kind info_hash num_pieces addr r w 
  >>= close

(* Creates a server that waits for connection from peers.
   After handshake, peer is initialized and added to Pwp. *)
let create_server_and_add_peers pwp torrent : unit Deferred.t =

  let open Torrent in
  let { info_hash; num_pieces } = torrent in

  let handler addr r w =
    info "incoming connection on server from peer %s" 
      (Socket.Address.Inet.to_string addr);
    add_connected_peer_and_close pwp `Peer_initiating info_hash num_pieces addr r w 
    >>| ignore_error addr 
  in 

  Server.start handler ~port:(G.port_exn ())

let add_peers_from_tracker pwp torrent addrs : unit Deferred.t =

  let open Torrent in 
  let { info_hash; num_pieces } = torrent in

  let add_peer addr = 
    let open Deferred.Or_error.Monad_infix in 
    let wtc = Tcp.to_inet_address addr in
    debug "try connecting to peer %s" (Socket.Address.Inet.to_string addr);
    Deferred.Or_error.try_with (function () -> Tcp.connect wtc)
    >>= fun (_, r, w) ->
    add_connected_peer_and_close pwp `Am_initiating info_hash num_pieces addr r w 
  in

  let f addr = add_peer addr >>| ignore_error addr in
  Deferred.List.iter ~how:`Parallel addrs ~f

(* TODO try to use option monad *)
let parse_uri f =

  let uri = Uri.of_string f in

  (* st should be of the form "urn:btih:hex_info_hash" *)
  let decode_xt st = 
    if not (String.length st = 49) then
      `Invalid_magnet
    else
      let hex_string = `Hex (String.sub st ~pos:9 ~len:40) in
      let info_hash = Hex.to_string hex_string |> Bt_hash.of_string in
      `Magnet info_hash
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

let process uri =

  let f = 
    match parse_uri uri with
    | `File f -> f 
    | `Magnet info_hash -> failwith "magnet not implemented yet"
    | `Other -> failwith "scheme error"
    | `Invalid_magnet -> failwith "invalid magnet"
  in

  (***** read torrent file *****)
  let t = try 
      Torrent.from_file f
    with
    | Sys_error _ -> Em.terminate (Em.wrong_file f)
    | Failure s -> Em.terminate (Em.not_bencode f)
    | Bencode_utils.Bencode_error -> Em.terminate (Em.wrong_bencode f)
    | ex -> raise ex
  in 

  let open Torrent in
  let { files_info; num_pieces; piece_length; torrent_name; total_length; 
        info_hash; pieces_hash; num_files } = t in

  (***** open all files  **********)

  (* TODO catch errors *)
  let%bind pers = Pers.create files_info num_pieces piece_length  in

  (**** read bitfield *************) 

  let bf_name = sprintf "%s/%s%s" (G.path ()) (Filename.basename torrent_name) 
      G.bitset_ext in

  let bf_len = Bitset.bitfield_length_from_size num_pieces in 

  let bitfield = 
    try
      Pers.read_bitfield bf_name bf_len  (* TODO don't deserve function in Pers *)
    with _ -> 
      info "can't read bitfield %s. Using empty bitfield" bf_name;
      Bitfield.empty bf_len
  in
  info "read bitfield %s" bf_name;
  let bitset = Bitset.of_bitfield bitfield num_pieces in

  (**** read routing table *****)
  (* TODO put this in main together with server *)

  let routing_table_name = sprintf "%s/%s" (G.path ()) G.routing_table_name in 
  let routing_table = 
    try
      In_channel.read_all routing_table_name
    with _ -> 
      info "can't read routing table %s. Using empty table" routing_table_name;
      ""
  in
  let decoded_table = 
    try
      Krpc.table_of_string routing_table 
    with _ -> 
      info "can't decode routing table %s. Using empty table" routing_table_name;
      [] 
  in

  let f (_, p) = Krpc.try_add p |> Deferred.ignore |> don't_wait_for in
  List.iter decoded_table ~f;

  (****** initialize File.t and retrieve pieces from disk *******)

  let file = File.create pieces_hash ~piece_length ~total_length in

  let read_piece i : unit Deferred.t =
    let p = File.get_piece file i in
    Pers.read_piece pers p 
    >>| fun () -> 
    if Piece.is_hash_ok p then File.set_piece_status file i `Downloaded
    else info "can't read piece %d from disk" i
  in

  Deferred.List.iter (Bitset.to_list bitset) ~f:read_piece 

  >>= fun () ->

  Print.printf "read %d%% from disk\n" (File.percent file);
  info "read %d/%d pieces" (File.num_downloaded_pieces file) num_pieces;
  debug "read from files: %s" (File.pieces_to_string file);

  (***** set up Pers (pipe for writing pieces) *****)

  (* this will be called when pipe is closed *)
  let finally () =
    (try
       Pers.write_bitfield bf_name (File.bitfield file)
     with 
       _  -> Print.printf "%s\n" (Em.can't_open bf_name));
    (try 
       let table = Krpc.table () in
       Out_channel.write_all routing_table_name ~data:(Krpc.table_to_string table);
       info "writing routing table to file %s" routing_table_name;
     with
     (* TODO print error in debug *)
       _ -> Print.printf "%s\n" (Em.can't_open routing_table_name)
    );


    Pers.close_all_files pers >>= fun () ->
    Print.printf "written %d%% to disk\n" (File.percent file);
    info "written %d/%d pieces" (File.num_downloaded_pieces file) num_pieces;
    debug "written to files: %s" (File.pieces_to_string file);
    exit 0
  in 

  don't_wait_for (Pers.init_write_pipe pers ~finally);

  Signal.handle Signal.terminating ~f:(fun _ -> Pers.close_pipe pers);

  (***** get list of peers ****)

  let%bind addrs = match%bind Tracker_client.query t with
    | Some addrs -> return addrs 
    | None -> Em.terminate (Em.tracker_error ())
  in
  let num_of_peers = List.length addrs in 
  Print.printf "tracker returned %d peers\n" num_of_peers;
  info "tracker replies with list of %d peers" num_of_peers;

  (******* create pwp and add peers ****)

  (* There are two types of peers. 
     - those that we contact  (got their addresses from the tracker
     - those that contact us (via the server) *)

  let pwp = Pwp.create t file pers in

  (* TODO move this to main *)
  let server = if G.is_server () then 
      create_server_and_add_peers pwp t >>= never
    else 
      Deferred.unit
  in

  let peers = add_peers_from_tracker pwp t addrs in 

  (* this should not be determined, unless all peers and the server fail.
     We may as well use [never] *)
  Deferred.all_unit [server; peers]






