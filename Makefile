.PHONY: default unit test clean bittorrent

bittorrent:
	jbuilder build main/src/main.bc 

all: 
	jbuilder build @install
	
client:
	jbuilder build main/src/main.bc 

client_repl:
	jbuilder build main_repl/src/main.bc 

server:
	jbuilder build tracker/src/tracker_server.bc 

unit:
	jbuilder runtest

clean:
	jbuilder clean

README.html: README.md
	pandoc -c style.css -f markdown_github < README.md > README.html
