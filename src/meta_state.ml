open Core
open Async
open Log.Global

type t = {
  torrent : Torrent.info;
  file : File.t;
  pers : Pers.t;
}

