.PHONY: lib demo

all: lib demo

lib:
	$(MAKE) -C lib

demo:
	pandoc -f markdown -t html README.md -o docs/readme.html
	cp lib/main.js docs/js/mlts.js

