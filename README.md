# A simple bittorrent client in OCaml/Async

This aims to be a simple, well documented and functional bittorrent client written with the Async framework. It is still in an early stage but it is already possible to download files from torrents. The code is (I believe) understandable and documented but can be improved in many ways.

### Usage

You need to install some external libraries with `opam` (`core`, `async`, `cohttp`, `bencode`, `sha`, `uri` - see `_tags` file). Then compile with `make` 

Usage:

    > ./main.byte -help
    Download torrent file

      main.byte FILE

    === flags ===

      -p             set download path
      [-l]           set server mode with port
      [-v]           verbose (level = 1 or 2)
      [-build-info]  print info about this build and exit
      [-version]     print the version of this build and exit
      [-help]        print this help text and exit
                     (alias: -?)

Optional server mode allows peers to contact the client.

Example of interaction:

    ./main.byte tests/torrents/NuTyX_x86_64-20170625.torrent -p download/ 
    read 68% from disk.
    tracker returned 6 peers
    hanshake with (tracker) peer 62.210.82.82:5366
    hanshake with (tracker) peer 185.21.216.195:59993
    hanshake with (tracker) peer 31.162.243.67:63347
    downloading from peer 31.162.243.67:63347
    downloaded 69%
    downloaded 70%
    downloaded 71%
    downloaded 72%
    ^Cwritten 72% to disk

The program starts with reloading the previous state of the downloaded files. `Ctrl^C` terminates the program and finish writing all downloaded data to file.

This caommand can be run with `make test` (`NuTyX_x86_64-20170625.torrent` is a legal linux distribution torrent). Several other torrents are on the repository for testing purpose but they may become obsolete in the future.

### How does bittorrent work?

You should read the bittorrent specification (see references below) but in a nutshell, this is how it works. It starts with a meta-information file (the *torrent file*) that contains information on the data to be downloaded as well as the url(s) of the *tracker(s)*. A tracker is an http-server that provides the addresses of the peers willing to share *pieces* of a *network file*. It is a contiguous sequence of bytes made of the concatenation of the individual files the user wishes to download. Once the peers are known, communication is established with some of them in order to request the precious pieces. At the same time, peers can query the client to request pieces they don't have. Peers can either contact the client directly if it has made its adress public via the tracker, or simply on the same connection the client has established with them

### Implementation

This is a high-level description. See ocamldoc for more details. Run `make doc` and open `.docdir/index.html`. 

Module `Main` decodes and checks the command-lines arguments and call `Start.process file`. This function runs the main steps of the program.

The first step is to decode the torrent file. It is binary encoded as [bencode](https://en.wikipedia.org/wiki/Bencode). We use the `bencode` library to decode it in module `Torrent`. Both single-file and multiple-file torrent format are implemented. 

The torrent gives various information, notably: 
* the tracker url
* the list of files and their length.
* a list of peers (via the `announce` and `announces-list` keys)

We compute from the torrent the *info hash* (SHA1 of the `info` section). It is a 20-bytes string that uniquely identifies the torrent. It is used to identify the torrent when querying the tracker, and for handshaking with the peers. The bencode may contain one (`announce` key) or several trackers (`announce-list`) urls. In the case of serveral trackers, we pick any responding tracker from the list.

The next step is to initialize the `File.t` data structure which store the network file. A `File.t` is essentially an array of `Piece.t`, plus 
the information needed to know the status of each pieces (e.g. downloaded or requested). From `File.t`, we can generate the serialized form of the list of pieces already downloaded, call the *bitfield* (see *Bitfield.t* and *Bitset.t*).

 Each `Piece.t` is furthermore divided into blocks (a `Bitset.t` field of `Piece.t` describes the blocks of a piece already downloaded). Note that the unit of ownership (advertised to other peers) is a piece (torrent-defined), and the unit of transmission is a block (client specific, commonly 16KB or 32KB).

The next step is to retrieve the data saved to disk in previous executions of the program. Two types of objects are saved to disk.
* the set of pieces already downloaded (we call it the *bitfield*) 
* the actual data  

This is saved in the directory given by path. The files are created with their final size at the first execution and content is updated as the application runs, except for the bitfield that is saved upon termination.

Reading/writing of pieces is done in module `Pers`. Each piece of the network file is mapped to a list of `Pers.segment` (file descriptor, offset, length). This has to be done carefully, especially for multiple files where a piece can be mapped to several files. Moreover, `Pers` sets up a writing "master thread" that processes sequentially the writing requests sent asynchronously through a pipe and make sure that closing happens only once all scheduled pieces write have been processed. 

We then query the tracker (`Tracker_client`) with a GET http request wth a few parameters, the most important ones are:
 * the info hash
 * the port the client listens to (if server is enabled)
The answer is again bencoded, and contains the list of peers.

Once we get the list of peers (`Peer.t`), we initiate the *peer wire protocol* with each of the peers. It is is a binary protocol that proceeds in two steps:

 * an initial handshake (one round-trip message exchange)
 * then (binary asynchronous) messages to get parts of the file.

Messages are implemented with type `Message.t` and serialized using `Bin_prot` module. The module `Pwp` implements the peer wire protocol.

The strategy to download/upload piece is simple and has to be improved. We consider that we are not choking any peer, and are interested in all peers. This means we respond to any incoming piece request. We ask pieces ordered by rarity to any peers (except the very slow ones that we discard). 

### Limitations

A lot remains to be done in order for this to be usable. It works on simple torrents but should be tested more extensively. However, the core elements are there and it should be easy to build upon them. 

Basic features are missing:
* Re-query the tracker to find more peers 
* Read parameters from json files
* Download several files concurrently
* A decent user interface
* Udp trackers
* Downloading of magnets via DHT

The last two items should be the most useful ones, if only for testing, as it's easier to download torrents that depend on them.

Regarding the implementation: 

* Implementation of the protocol. 
  * Still very basic and straighforward to see what downloading/uploading strategy to put in place. It requires some experimentation to find the right heuristic. 
* Lot of TODOs in the code, but mainly minor improvements
* Use more efficient datastructures for requesting strategy
* Automatic testing / continuous integration with travis 

### Resources and libs

* bittorrent [official](http://bittorrent.org/beps/bep_0000.html) and [unoffical](https://wiki.theory.org/index.php/Main_Page) specification
* Wireshark tool (packet sniffer, useful to debug protocol) and [bittorrent dissector](https://wiki.wireshark.org/BitTorrent)
* OCaml Core and Async ([0.9](https://ocaml.janestreet.com/ocaml-core/v0.9/doc/)) 
* [bin_prot](https://github.com/janestreet/bin_prot)
* [bencode](https://github.com/rgrinberg/bencode).
* [cohttp-async](https://github.com/mirage/ocaml-cohttp)
* [sha](https://github.com/vincenthz/ocaml-sha)
* some [tutorial](http://www.kristenwidman.com/blog/71/how-to-write-a-bittorrent-client-part-2) on bittorrent.
