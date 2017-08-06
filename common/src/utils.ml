open Core

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

