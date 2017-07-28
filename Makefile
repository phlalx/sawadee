.PHONY:	all clean debug test test_bitset server 

OCB_FLAGS = -tag bin_annot -use-ocamlfind 
OCB =	ocamlbuild $(OCB_FLAGS) -I src -I src_test

all: main tracker_server test_bitset test_krpc_packet README.html

main:
	$(OCB) main.byte

tracker_server:
	$(OCB) tracker_server.byte

test_bitset:
	$(OCB) test_bitset.byte

test_krpc_packet:
	$(OCB) test_krpc_packet.byte
	
doc:
	$(OCB) .docdir/index.html

unit: test_bitset test_krpc_packet
	./test_bitset.byte
	./test_krpc_packet.byte

clean: 
	$(OCB) -clean
	rm -f README.html

README.html: README.md
	pandoc -c style.css -f markdown_github < README.md > README.html

# set the torrent file you want to download
TEST_REMOTE = tests/torrents/NuTyX_x86_64-20170625.torrent
# TEST_REMOTE = tests/torrents/This_Week_@NASA,_July_7,_2017[V004848568].mp4.torrent
VERBOSE = -v 1 # set to see execution trace in test
DOWNLOAD_PATH = -p download/

test: tracker_server main 
	./main.byte $(TEST_REMOTE) $(DOWNLOAD_PATH) $(VERBOSE)
