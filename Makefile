.PHONY:	all clean debug test test_bitset server 

OCB_FLAGS = -tag bin_annot -use-ocamlfind # -cflag -safe-string 
OCB =	ocamlbuild $(OCB_FLAGS) -I src

TEST_REMOTE = tests/torrents/ubuntu-17.04-desktop-amd64.iso.torrent
#TEST_REMOTE = tests/torrents/NuTyX_x86_64-20170625.torrent

all: byte tracker_server.byte

byte:
	$(OCB) main.byte

server:
	$(OCB) tracker_server.byte

test: byte
	./main.byte $(TEST_REMOTE)
	
doc:
	$(OCB) .docdir/index.html

# unit test of bitset module
test_bitset.byte:
	$(OCB) test_bitset.byte

unit: test_bitset.byte
	./test_bitset.byte

clean: 
	$(OCB) -clean
	rm -f README.html
	rm -rf .docdir

README.html: README.md
	pandoc -c style.css -f markdown_github < README.md > README.html

