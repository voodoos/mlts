# MLTS Web
A web interface to experiment with new concepts in MLTS.
You can access a demo of this interface here.

## Build dependencies
Before attempting to build the project you should make sure that you have the following dependencies installed :

- [Opam](https://opam.ocaml.org/doc/Install.html) : 
```bash
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin
```
- [OCaml](https://caml.inria.fr/) 4.06.0 :  
```bash
opam switch install 4.06.0
```
- [Menhir](http://gallium.inria.fr/~fpottier/menhir/), [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml), [base64](https://github.com/mirage/ocaml-base64) :  
```bash
opam install menhir js_of_ocaml js_of_ocaml-ppx base64
```
- [Elpi](https://github.com/LPCIC/elpi) :  
```bash
opam pin add elpi https://github.com/LPCIC/elpi.git
```

