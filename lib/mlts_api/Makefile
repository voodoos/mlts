
.PHONY: 	all test exe

all:
	ocamlbuild mlts_API.cma

test: exe
	../_build/default/mlts_API/mlts_API.exe

exe:
	dune build mlts_API.exe
