.PHONY: lib demo

all: lib demo

lib:
	$(MAKE) -C lib

demo:
	cp lib/main.js docs/js/mlts.js

