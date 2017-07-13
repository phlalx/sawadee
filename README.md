# OCaml-torrent (work in progress)

This is a toy bittorrent client for the sake of familializing myself with `Core` and `Async` APIs.

### Usage

You will need to install some external libraries with `opam` (notably `core`, `async`, `cohttp`, `bencode`, `sha`, `uri`).

Compile with `make` then run `./main.byte FILE` to start the program. `FILE` is a torrent file. (e.g. `ubuntu-17.04-desktop-amd64.iso.torrent`).

### How does it work?

This my current understanding of the protocol and is subject to change!

A bittorrent file is encoded as [bencode](https://en.wikipedia.org/wiki/Bencode). First step is to decode the torrent file (we use the `bencode` library) and extract the relevant information (including the server `tracker` URL). Next step is to http-query the tracker to retrieve the list of peers sharing (parts of) the file. One important parameter of the query is the 20-bytes string `sha1` value of the info section of the torrent file. This will identify the file in subsequent queries.

Once we know the list of peers, we'll talk with each of them using a binary protocol. This is done in two steps.
 * an initial handshake (one round-trip message exchange)
 * then (binary asynchronous) messages to get parts of the file.

Current stage of the project: 
 * several peers can be queried concurrently
 * The handshake is functional (in `App_layer`).
 * Binary messages are implemented (via serialization of `Message.t`). 
 * Per-peer state (including socket read/write, choked, interested status...) is maintained in `Peer`.
 * A `File.t` is divided in `Piece.t`. Each `Piece.t` is furthermore divided in blocks (Bitset.t describes the blocks of a piece already downloaded). 
 * In the application layer `App_layer`, after the handshake, two services are launched. 
 * One continuously wait for peer messages (block of files, information on pieces ownership, request to download...). 
 * The other one requests for pieces. At the moment, only peers that are not choking with less than a few pending request are considered. Among them, we ask for pieces that not yet requested or downloaded without any particular strategy.

This is still very basic and not functional, although almost there :). A lot remains to be done. Immediate next steps for this to be usable.
* re-request block if needed  
* save file to disk 
* test with various torrent files
* answer requests from peers (pieces, bitfield) 
* Improve requesting strategy (e.g. rare pieces first, peer responsivness)

Morever, the code can be much improved.
* don't request blocks at regular interval (polling) but based on event occurences (e.g. new pieces availables)
* better resource management (close connections...)
* Use more efficient datastructures
* Better use of the existing API. 
* Rework the module separations. There are invariant that span several modules that should be identified and if possible encapsulated. 

### Resources and libs

* bittorrent [official](http://bittorrent.org/beps/bep_0000.html) and [unoffical](https://wiki.theory.org/index.php/Main_Page) specification
* Wireshark tool (packet sniffer, useful to debug protocol) and [bittorrent dissector](https://wiki.wireshark.org/BitTorrent)
* OCaml Core and Async ([0.9](https://ocaml.janestreet.com/ocaml-core/v0.9/doc/)) 
* [bin_prot](https://github.com/janestreet/bin_prot)
* [bencode](https://github.com/rgrinberg/bencode).
* [cohttp-async](https://github.com/mirage/ocaml-cohttp)
* [sha](https://github.com/vincenthz/ocaml-sha)
* some [tutorial](http://www.kristenwidman.com/blog/71/how-to-write-a-bittorrent-client-part-2) on bittorrent.
