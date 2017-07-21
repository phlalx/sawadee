.PHONY:	all clean debug test test_bitset server run_server

OCB_FLAGS = -tag bin_annot -use-ocamlfind # -cflag -safe-string 
OCB =	ocamlbuild $(OCB_FLAGS) -I src

#TEST_LOCAL = tests/torrents/multifiles_localhost_6969.torrent
TEST_LOCAL = tests/torrents/singlefile_localhost_6969.torrent
#TEST_REMOTE = tests/torrents/ubuntu-17.04-desktop-amd64.iso.torrent
TEST_REMOTE = tests/torrents/NuTyX_x86_64-20170625.torrent

all: test.byte tracker_server.byte

test.byte:
	$(OCB) main.byte

tracker_server.byte:
	$(OCB) tracker_server.byte

# test simple torrent file (single-file), many seeders 
test: main.byte
	./main.byte $(TEST_REMOTE)

# run server tracker for local testing
run_server: tracker_server.byte
	./tracker_server.byte

# run server first, then local_test1 
local_test1: main.byte
	./main.byte -l 6000 $(TEST_LOCAL)

# run server and local_test1 first, then local_test2
local_test2: main.byte
	./main.byte -p download2 $(TEST_LOCAL)
	
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
	rm -f download/* download2/*

README.html: README.md
	pandoc -c style.css -f markdown_github < README.md > README.html

