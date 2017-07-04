# OCaml-torrent (work in progress)

This is a toy bittorrent client for the sake of familializing myself with `Core` and `Async` APIs.

### Usage

You will need to install some external libraries with `opam` (notably `core`, `async`, `cohttp`, `bencode`, `sha`, `uri`).

Compile with `make` then run `./main.byte FILE` to start the program. `FILE` is a torrent file. (e.g. `ubuntu-17.04-desktop-amd64.iso.torrent`).

### How does it work?

This describes the current stage of the project.

A bittorrent file is encoded as [bencode](https://en.wikipedia.org/wiki/Bencode). First step is to decode the torrent file (we use the `bencode` library) and extract the relevant information (including the server `tracker` URL). Next step is to http-query the tracker to retrieve the list of peers sharing (parts of) the file. One important parameter of the query is the 20-bytes string `sha1` value of the info section of the torrent file. This will identify the file in subsequent queries.

Once we know the list of peers, we'll contact each of them using a binary protocol. This is done in two steps.
 * an initial handshake (one round-trip message exchange)
 * then queries to get parts of the file. 

At that stage, only the handshake is functional (with exactly one peer). The binary messages are implemented (via serialization of `Message.t`) but the protocol remains to be done. 

### Resources and libs

* bittorrent [official](http://bittorrent.org/beps/bep_0000.html) and [unoffical](https://wiki.theory.org/index.php/Main_Page) specification
* Wireshark tool (packet sniffer, useful to debug protocol) and [bittorrent dissector](https://wiki.wireshark.org/BitTorrent)
* OCaml Core and Async ([0.9](https://ocaml.janestreet.com/ocaml-core/v0.9/doc/)) 
* [bin_prot](https://github.com/janestreet/bin_prot)
* [bencode](https://github.com/rgrinberg/bencode).
* [cohttp-async](https://github.com/mirage/ocaml-cohttp)
* [sha](https://github.com/vincenthz/ocaml-sha)
* some [tutorial](http://www.kristenwidman.com/blog/71/how-to-write-a-bittorrent-client-part-2) on bittorrent.
