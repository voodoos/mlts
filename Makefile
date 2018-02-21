.PHONY: lib demo

PYTHON=python3.6

all: lib demo

lib:
	$(MAKE) -C lib

demo: 
	pandoc -f markdown -t html README.md -o docs/readme.html
	cp lib/main.js docs/js/mlts.js

run: 
	(cd docs && $(PYTHON) -m http.server)
