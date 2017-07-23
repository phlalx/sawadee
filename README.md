# A simple Bittorrent client in OCaml/Async

This aims to be a simple, well documented and functional bittorrent client written with the Async framework.

But it is not quite there yet :) This is an ongoing project and is quickly changing.

### Usage

You will need to install some external libraries with `opam` (notably `core`, `async`, `cohttp`, `bencode`, `sha`, `uri` - see `_tags` file).

Compile with `make` then run `./main.byte FILE` to start the program. `FILE` is a torrent file. Several legal torrent files are in the archive for testing purpose but they may get obsolete in the future.

### How does it work?

You should read the bittorrent specification (see references below) but in a nutshell, this is how it works. It all starts with a meta-information file (the *torrent file*) that contains information on the data to be downloaded as well as the url(s) of the *tracker(s)*. A tracker is an http-server that provides the addresses of the peers willing to share *pieces* of the data. This data can be seen as a single file from the client perspective. We call it the *network file* (this is not the terminology used in the spec). It is made of the concatenation of the individual files the user wishes to download. Once the peers are known, communication is established with some of them in order to request the precious pieces. At the same time, peers can query the client to request pieces they don't have. 

### Implementation

This is a high-level description. The ocamldoc should be more precise and up-to-date.

The first step of the program is to decode the torrent file. It is binary encoded as [bencode](https://en.wikipedia.org/wiki/Bencode) format). We use the `bencode` library to decode it, this is done in module `Torrent`. Both single-file and multiple-file torrent format are implemented. 

The torrent gives various information, notably the tracker url, the list of files and their length. We compute from the torrent the *info hash* which the SHA1 of the info section. It is a 20-bytes string that uniquely identifies the torrent. It is used to identify the network file when querying the tracker, and for handshaking with the peers (see below). Note the bencode may contain one (`announce` key) or several trackers (`announce-list`) urls. Annouce list is only partialy supported (doesn't quite conform to the spec, we pick any responding tracker from the list). 

We query the tracker (`Tracker_client`) with a simple GET http request whose main parameter is the info-hash. We use the `uri` library to create the request. The answer is again bencoded, and contains the list of peers.

When we got the list of peers, we initiate the *peer wire protocol* with each of the peers (`Peer.t`). It is is a binary protocol that proceeds in two steps:

 * an initial handshake (one round-trip message exchange)
 * then (binary asynchronous) messages to get parts of the file.

Messages are defined by type `Message.t` and serialized using `Bin_prot` core module. The module `Pwp` implements the peer wire protocol initializes the `File` datastructure (to store the network file), and the persistence module `Pers`. 
Per-peer state (including socket reader/writer, protocol state, pending requests) is maintained in `Peer.t`.

 A `File.t` is essentially an array of `Piece.t` plus a set of downloaded pieces (`Bitset.t`). Following the spec terminology, we call the serialized version of this set the *bitfield* (type `Bitfield.t`). It is simply an array of bits defining the possession of each individual piece.  Each `Piece.t` is furthermore divided into blocks (a `Bitset.t` field of `Piece.t` describes the blocks of a piece already downloaded). Note that the unit of ownership (advertised to other peers) is a piece (torrent-defined), and the unit of transmission is a block (client specific, commonly 16KB or 32KB).

Two types of objects are saved to disk. One is a bitfield of the pieces already downloaded (so we can resume downloading after quitting the client), and the other are the actual files.
Persistence is dealt with in module `Pers`.

We map each piece of the network file to a list of `Pers.segment` (file descriptor, offset, length). This has to be done carefully, especially for multiple files where a piece can be mapped to several files. Moreover, `Pers` sets up a writing "master thread" that processes sequentially the writing requests sent asynchronously through a pipe. 

 * There is a message loop that wait and process peer messages
 * Another one requests for random pieces by polling the number of pending requests and the availability of new pieces. 

Besides, requests that have been pending for n seconds are canceled. To check these requests, we poll the peers regularly to see if they are idle. In that case, we ignore theem and mark the pieces as non-requested. They will be re-requested to other peers.

### TODO 

What I'd like to complete in the near future. 

* Improve the implementation of the protocol
* improve the code
  * more consistent error management 
  * module decomposition can be improved
  * review possible race conditions and async issues 
* resource management
  * better handling of socket connections (freeing of fd)
  * be more careful with buffer allocation
* Dealing with DHT and magnet 
* Automatic testing 

Less urgent.

* Use more efficient datastructures in requesting
* Read parameters from json files
* Re-query the tracker to find more peers 
* download several files concurrently

### Resources and libs

* bittorrent [official](http://bittorrent.org/beps/bep_0000.html) and [unoffical](https://wiki.theory.org/index.php/Main_Page) specification
* Wireshark tool (packet sniffer, useful to debug protocol) and [bittorrent dissector](https://wiki.wireshark.org/BitTorrent)
* OCaml Core and Async ([0.9](https://ocaml.janestreet.com/ocaml-core/v0.9/doc/)) 
* [bin_prot](https://github.com/janestreet/bin_prot)
* [bencode](https://github.com/rgrinberg/bencode).
* [cohttp-async](https://github.com/mirage/ocaml-cohttp)
* [sha](https://github.com/vincenthz/ocaml-sha)
* some [tutorial](http://www.kristenwidman.com/blog/71/how-to-write-a-bittorrent-client-part-2) on bittorrent.
