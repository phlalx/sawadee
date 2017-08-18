
type t = 
  (* only before nf is set *)
  | Support_meta
  | Tinfo of Torrent.info
  | Bye
  (* only after nf is set *)
  | Piece of int

val to_string : t -> string