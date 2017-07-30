.PHONY:	all clean debug test test_bitset server 

OCB_FLAGS = -tag bin_annot -use-ocamlfind 
OCB =	ocamlbuild $(OCB_FLAGS) -I src -I src_test


all: main tracker_server test_bitset test_krpc_packet

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

