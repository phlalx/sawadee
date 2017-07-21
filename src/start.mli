(** entry point for downloading files from a torrent file. *)
open Core
open Async

(* [process s] downloads torrent file [s] *)
val process : string -> unit Deferred.t