.PHONY: default unit test clean

default:
	jbuilder build src/main.bc 

all:
	jbuilder build src/main.bc src/test_krpc_packet.bc src/test_bitset.bc src/tracker_server.bc

unit:
	jbuilder runtest

clean:
	jbuilder clean

README.html: README.md
	pandoc -c style.css -f markdown_github < README.md > README.html
