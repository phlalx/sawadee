open Core
open Async

type t = Socket.Address.Inet.t 

let create = Socket.Address.Inet.create

let to_string = Socket.Address.Inet.to_string

let port = Socket.Address.Inet.port 

let addr = Socket.Address.Inet.addr 