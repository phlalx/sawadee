.PHONY: all unit clean

all: 
	dune build @install

unit:
	dune runtest

clean:
	dune clean

README.html: README.md
	pandoc -c style.css -f markdown_github < README.md > README.html
