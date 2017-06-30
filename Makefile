.PHONY:	all clean debug test byte native

OCB_FLAGS = -tag bin_annot -use-ocamlfind 
OCB =	ocamlbuild $(OCB_FLAGS)

all: byte

native:
	$(OCB) main.byte

byte:
	$(OCB) main.byte

test: byte
	./main.byte ubuntu-17.04-desktop-amd64.iso.torrent

debug:
	$(OCB) -tag debug main.byte

clean:
	$(OCB) -clean
	rm -f README.html

README.html: README.md
	pandoc -c style.css -f markdown_github < README.md > README.html
