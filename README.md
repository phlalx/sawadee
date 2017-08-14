# Sawadee: A simple bittorrent client in OCaml/Async

Sawadee is a toy implementation of the bittorrent protocol written in OCaml using the Async concurrency framework. It provides a bittorrent library as well as a few executable front-ends.

## Installation

The easiest way to install Sawadee is through `opam`.

    opam install sawadee

## Usage

## Limitations

## Resources and libs

* bittorrent [official](http://bittorrent.org/beps/bep_0000.html) and [unoffical](https://wiki.theory.org/index.php/Main_Page) specification
* Wireshark tool (packet sniffer, useful to debug protocol) and [bittorrent dissector](https://wiki.wireshark.org/BitTorrent)
* OCaml Core and Async ([0.9](https://ocaml.janestreet.com/ocaml-core/v0.9/doc/)) 
* [bin_prot](https://github.com/janestreet/bin_prot)
* [bencode](https://github.com/rgrinberg/bencode).
* [cohttp-async](https://github.com/mirage/ocaml-cohttp)
* [sha](https://github.com/vincenthz/ocaml-sha)
