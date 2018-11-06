
.PHONY: 	all test exe

all:
	ocamlbuild mlts_API.cma

test: exe
	../_build/default/mlts_API/mlts_API.exe

toElpi: exe
	../_build/default/mlts_API/mlts_API.exe | tee ../data/core/progs.elpi


exe:
	dune build mlts_API.exe
