# OCaml-torrent

This is a toy bittorrent client for the sake of familializing myself with Core and Async APIs 
and torrent/magnet protocol. Not intended to be usable.

### Usage

You will need to install some external libraries with `opam` (notably `core`, `async`, `cohttp`, `bencode`, `sha`).

`make` to compile, and `./main.byte FILE` where `FILE` is a torrent file (e.g. ./main.byte ubuntu-17.04-desktop-amd64.iso.torrent). 

### How does it work?

This reflects my current understanding of bittorrent and the current state of the client.

A bittorrent file is encoded as [bencode](https://en.wikipedia.org/wiki/Bencode). First step is to decode the torrent file (we use the `bencode` library) and extract the relevant information (including the server `tracker`). Next step is to http-query the tracker to retrieve the list of peers sharing (parts of) the file. One important parameter of query is the 20-bytes string `sha1` value of the info section of the torrent file. This will identify the file in subsequent queries.

Once we know the list of peers, we'll contact each of them using a binary protocol. First, a handshake, then queries to get parts of the file. At that stage, only the handshake is functional.

### Resources and libs

* [bittorrent specification](http://bittorrent.org/beps/bep_0000.html).
* Wireshark tool (packet sniffer, useful to debug protocol) and [bittorrent dissector](https://wiki.wireshark.org/BitTorrent)
* OCaml Core and Async ([0.9](https://ocaml.janestreet.com/ocaml-core/v0.9/doc/)) 
* [bin_prot](https://github.com/janestreet/bin_prot)
* [bencode](https://github.com/rgrinberg/bencode).
* [cohttp-async](https://github.com/mirage/ocaml-cohttp)
* [sha](https://github.com/vincenthz/ocaml-sha)



