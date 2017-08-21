.PHONY: main all main_repl tracker unit clean test test_repl test_rpc

main:
	jbuilder build main/bin/main.bc 

all: 
	jbuilder build @install
	
main_repl:
	jbuilder build main_repl/bin/main.bc 

tracker:
	jbuilder build tracker/bin/main.bc 

unit:
	jbuilder runtest

clean:
	jbuilder clean

README.html: README.md
	pandoc -c style.css -f markdown_github < README.md > README.html
