EXECDIR=../_build/default/mlts_api

.PHONY: 	all test exe

all:
	ocamlbuild mlts_API.cma

test: exe
	$(EXECDIR)/mlts_API_test.exe

toElpi: exe
	$(EXECDIR)/mlts_API_test.exe | tee ../data/core/progs.elpi

exe:
	dune build mlts_API_test.exe
