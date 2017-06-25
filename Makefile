all:
	corebuild main.native

clean:
	corebuild -clean
	rm -f README.html

README.html: README.md
	pandoc -c style.css -f markdown_github < README.md > README.html
