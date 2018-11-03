.PHONY: translator data main.byte js core core-sync

PYTHON ?= python3.6

all: data js

translator:
	$(MAKE) -C mlts_api

data:
	cd data && $(PYTHON) ../pack_data.py > ../data.ml

js: 
	dune build main.bc.js --profile release

core: core-sync
	cd data/core && ../../to_elpi.sh

core-sync:
	./sync_core.sh
