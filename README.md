# MLTS Web
A web app designed to try and play with the  NabMLTS (to be renamed) programming language.
Source is hosted on [GitHub](https://github.com/voodoos/mlts) as is the [demo](http://voodoos.github.io/mlts/).

## Known issues and missing features
This work is highly experimental and you should expect bugs to appear. If so please report them on the [issue tracker](https://github.com/voodoos/mlts/issues).

- There is no syntactic sugar for non-empty lists. Use `a::b::c::[]` instead of `[a;b;c]`.
- Error messages are not always meaningfull of even shown.


## Concrete syntax
MLTS concrete syntax is based on OCaml. A program written in MLTS not using the new constructs `nab in`, `new in`, `\` and `=>` should compile with the `ocamlc` compiler.

## How it works
Our protoype interpreter for MLTS is written in λProlog. When you press the "Run" button the MLTS concrete syntax is transpilled to it's λProlog counterpart and [Elpi](https://github.com/LPCIC/elpi), an embeddable λProlog interpreter written in OCaml, runs it.

All this happens locally in your browser thanks to the js_of_ocaml [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml).

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

## Credits
The "backend" is powered by OCaml, λProlog, Elpi and js_of_ocaml.

The frontend uses a standard mix of Bootsrap and Jquery but also the Ace code editor, highlight-js for additionnal syntactic coloring and open-iconic for icons.

