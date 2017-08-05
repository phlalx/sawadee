open Core
open Async

(** [process f] launches the whole process of downloading files from
    uri [u]. 

    u can be a file (no scheme) or a magnet (scheme = magnet)

  This includes:
  - retrieving persistent data (bitfield and pieces of files already downloaded)
  - connecting with the tracker to get peers do share data with
  - launching server if server mode is enabled
  - communicating with peers 
  - registering termination handler to save all data on termination signal. 

  The returned deferred shouldn't be determined if server is running. Otherwise,
  it will be determined iff all the peers fail or leave. This is because we only
  query the tracker once. *)

val process_string : string -> Bt_hash.t Deferred.t

val process_magnet : Bt_hash.t -> Bt_hash.t Deferred.t