# A simple Bittorrent client in OCaml/Async

The aim of this project is to have a simple, well documented and functional bittorrent client written using the Async framework.

This is an ongoing project and is quickly changing.

### Usage

You will need to install some external libraries with `opam` (notably `core`, `async`, `cohttp`, `bencode`, `sha`, `uri` - see `_tags` file).

Compile with `make` then run `./main.byte FILE` to start the program. `FILE` is a torrent file. Several legal torrent files are in the archive for testing purpose but they may get obsolete in the future.

### How does it work?

You should read the bittorrent specification (see references below) but in a nutshell, this is how it works. 

It all starts with a meta-information file (the *torrent file*) that contains information on the data to be downloaded as well as the url(s) of the *tracker(s)*. A tracker is an http-server that provides the addresses of the peers willing to share *pieces* of the data. This data can be seen as a single file from the client perspective. We call it the *network file* although this is not the terminology used in the spec). It is made of the concatenation of the individual files the user wishes to download.

The first step of the program is to decode the torrent file. It is binary encoded as [bencode](https://en.wikipedia.org/wiki/Bencode) format). We use the `bencode` library to decode it, this is done in module `Torrent`. A special value to be computed from the torrent is the *SHA1* hash (the *info hash*) of a section of the torrent file. It is a 20-bytes string that uniquely identifies the torrent and is used in some parts of the protocol. 

The next step is to create the structures that will contain the network file. These are module `File` and `Piece`. A `File.t` is essentially an array of `Piece.t` plus a set of downloaded pieces (`Bitset.t`). Following the spec terminology, we call the serialized version of this set the bitfield (type `Bitfield.t`). It is a sequence of bits (1 if piece is downloaded, 0 otherwise). 

Once this structure is initialized we create or retrieve the persistent data (the bitfield and the pieces already downloaded) and fill the `File.t` and `Piece.t` accordingly. Persistent data are file represented by type `PFile.t`. Note that  we need to map precisely each piece of the network file to a list of pfiles (file descriptor, offset, length). 

Once this is done, we query the tracker (`Tracker_client`) with a simple GET http request whose main parameter is the info-hash. We use the `uri` library to create the request. The answer is again bencoded, and contains the list of peers.

Then we initialize the *peer protocol* with an empty set of peers, and we add each of the peers. The peer protocol talks with each of them using a binary protocol. There are two steps.

 * an initial handshake (one round-trip message exchange)
 * then (binary asynchronous) messages to get parts of the file.

### Various notes 
 * The module `App_layer` implements the peer protocol.
 * both single-file and multiple-file format are correctly read
 * annouce-list is (partialy) supported (not quite follows the spec)
 * Binary messages are implemented using `Bin_prot` lib via serialization of `Message.t`. 
 * Per-peer state (including socket read/write, protocol state, pending requests) is maintained in `Peer.t`.
 * A `File.t` is divided in `Piece.t`. Each `Piece.t` is furthermore divided in blocks (a `Bitset.t` field of `Piece.t` describes the blocks of a piece already downloaded). 
 * In `App_layer`, after the handshake, several *services* (repeating async jobs) are launched. 
 * There is a message loop that wait and process peer messages
 * Another one requests for random pieces by polling the number of pending requests and the availability of new pieces. This can be much improved.
* Requests that have been pending for n seconds are canceled. When that happens, we ignore the peer and mark the pieces as non-requested.
* File/Bitset is saved to disk on exit (termination signal) and restored at startup. This doesn't work propery yet.

### TODO 

What I'd like to complete.

* Understand better the system aspects.
  * how Async plays with the file descriptors and the system threads
  * debug the persistence layer
  * resource management (better handling of socket connections)
* deal correctly with requests from peers (for some reasons, I don't get any request)
* document and clean up the code
* more consistent error managment and more defensive from malicious peers
* minimal testing
* Rework the module interfaces
  * `PFile` and move the persistence aspect from `File`. 
  * There are invariant that span several modules that should be documented and if possible encapsulated  

### Limitations

Not sure I'll try to work on this.

* Better usage of resources. A lot of string get allocated but this could be prevented by making a better of the `Substring` lib. 
* Use more efficient datastructures
* We query the tracker only once but we could have a more dynamic set of peers
* Server mode when using public IP 
* Dealing with DHT and magnet
* Improve requesting strategy (based on peer information, rarity of pieces, and don't use polling for piece requesting or to check peer reactivy)
* Automatic testing / benchmarking
* Read parameters from json files

### Resources and libs

* bittorrent [official](http://bittorrent.org/beps/bep_0000.html) and [unoffical](https://wiki.theory.org/index.php/Main_Page) specification
* Wireshark tool (packet sniffer, useful to debug protocol) and [bittorrent dissector](https://wiki.wireshark.org/BitTorrent)
* OCaml Core and Async ([0.9](https://ocaml.janestreet.com/ocaml-core/v0.9/doc/)) 
* [bin_prot](https://github.com/janestreet/bin_prot)
* [bencode](https://github.com/rgrinberg/bencode).
* [cohttp-async](https://github.com/mirage/ocaml-cohttp)
* [sha](https://github.com/vincenthz/ocaml-sha)
* some [tutorial](http://www.kristenwidman.com/blog/71/how-to-write-a-bittorrent-client-part-2) on bittorrent.
