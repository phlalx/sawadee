.PHONY: all unit clean

all: 
	jbuilder build @install

unit:
	jbuilder runtest

clean:
	jbuilder clean

README.html: README.md
	pandoc -c style.css -f markdown_github < README.md > README.html
