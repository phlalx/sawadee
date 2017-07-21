.PHONY:	all clean debug test test_bitset server run_server

OCB_FLAGS = -tag bin_annot -use-ocamlfind # -cflag -safe-string 
OCB =	ocamlbuild $(OCB_FLAGS) -I src

#TEST_REMOTE = tests/torrents/ubuntu-17.04-desktop-amd64.iso.torrent
TEST_REMOTE = tests/torrents/NuTyX_x86_64-20170625.torrent

all: test.byte tracker_server.byte

test.byte:
	$(OCB) main.byte

tracker_server.byte:
	$(OCB) tracker_server.byte

test: main.byte
	./main.byte $(TEST_REMOTE)
	
doc:
	$(OCB) .docdir/index.html

# unit test of bitset module
test_bitset.byte:
	$(OCB) test_bitset.byte

unit: test_bitset.byte
	./test_bitset.byte

clean: clean_torrent
	$(OCB) -clean
	rm -f README.html
	rm -rf .docdir

clean_torrent:
	rm -rf download*

README.html: README.md
	pandoc -c style.css -f markdown_github < README.md > README.html

