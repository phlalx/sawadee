(* http://www.bittorrent.org/beps/bep_0020.html 
   https://wiki.theory.org/index.php/BitTorrentSpecification
*)

include Hash_id.Id

open Core

let client s = 
  let p = String.prefix s 3 in
  match p with
  | "-UT" -> "utorrent "
  | "-DE" -> "deluge"
  | "-sW" -> "sawadee"
  | "-BC" -> "bitcomet"
  | "-AZ" -> "azuereus"
  | "-BT" -> "bittorrent"
  | "-BL" -> "bitblinder"
  | "-lt" -> "libtorrent"
  | "-LT" -> "libtorrent"
  | "-UM" -> "utorrent mac"
  | "-SD" -> "thunder"
  | "-qB" -> "qBittorrent"
  | "-MG" -> "qBittorrent"
  | "TIX" -> "tixati"
  | _ -> "unknown" ^ p

let to_string_hum s = (to_string_hum s) ^ "/" ^ (client s)

let sexp_of_t x = Sexp.Atom (to_string_hum x)