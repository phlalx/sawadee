.PHONY:	all clean debug test byte native test_bitset

OCB_FLAGS = -tag bin_annot -use-ocamlfind # -cflag -safe-string 
OCB =	ocamlbuild $(OCB_FLAGS)

all: byte

native:
	$(OCB) main.byte

byte:
	$(OCB) main.byte

test: byte
	./main.byte torrents/ubuntu-17.04-desktop-amd64.iso.torrent

debug:
	$(OCB) -tag debug main.byte

doc:
	$(OCB) .docdir/index.html

test_bitset:
	$(OCB) test_bitset.byte

unit: test_bitset
	./test_bitset.byte

clean:
	$(OCB) -clean
	rm -f README.html
	rm -rf .docdir

README.html: README.md
	pandoc -c style.css -f markdown_github < README.md > README.html

server:
	$(OCB) server.byte
