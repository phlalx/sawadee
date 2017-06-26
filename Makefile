all:
	ocamlbuild -use-ocamlfind main.native

clean:
	ocamlbuild -clean
	rm -f README.html

README.html: README.md
	pandoc -c style.css -f markdown_github < README.md > README.html
