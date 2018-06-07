# TryMLTS
A web app designed to try and play with the MLTS programming language.
Source is hosted on [GitHub](https://github.com/voodoos/mlts){target="_blank"} as is the [demo](http://voodoos.github.io/mlts/){target="_blank"}. You can also check the [latest draft](http://voodoos.github.io/mlts/mlts-paper.pdf){target="_blank"} of the paper describing MLTS.


## The λProlog interpreter
Our prototype interpreter is written in λProlog. Its source is located in the [lib/data/core folder](https://github.com/voodoos/mlts/tree/master/lib/data/core){target="_blank"}. The more interesting parts are:

- The [type checking algorithm](https://github.com/voodoos/mlts/blob/master/lib/data/core/typing.mod){target="_blank"} 
- The [evaluation algorithm](https://github.com/voodoos/mlts/blob/master/lib/data/core/eval.mod){target="_blank"}

## Known issues and missing features
This work is highly experimental and you should expect bugs to appear. If so please report them on the [issue tracker](https://github.com/voodoos/mlts/issues){target="_blank"}.

- There is no syntactic sugar for non-empty lists. Use `a::b::c::[]` instead of `[a;b;c]`.
- Error messages are not always meaningfull of even shown.
- There is no `_` wildcard to match anything but you can use a pattern variable like `iWillMatchAnything`.
- There is no way to declare mutually recursive functions but mutually rescursive types are accepted.


## Concrete syntax
MLTS concrete syntax is based on OCaml. A program written in MLTS not using the new constructs `nab in`, `new in`, `\` and `=>` should compile with the `ocamlc` compiler.

- Datatypes can be extended to contain new *nominal*
  constants and the `new X in body` program phrase provides a
  binding that declares that the nominal `X` is new within the
  lexical scope given by `body`.

- The `\` isan infix symbol that is used to form an abstraction of a nominal
  over its scope.  For example, `X\ body` is a syntactic
  expression that hides the nominal `X` in the scope `body`.
  Thus the backslash *introduces* an abstraction.  The `@`,
  conversely, *eliminates* an abstraction: for example, the
  expression `(X\body) @ Y` denotes the result of substituting
  the abstracted nominal `X` with the nominal `Y` in
  `body`.  Expressions involving `@` are restricted to be of
  the form `m @ X1 ... Xj` where `m` is a pattern (match)
  variable and `X1, ..., Xj` are nominals bound
  within the scope of the binding off `m`.

- A new typing constructor `=>` is used to type bindings
  within term structures.  This constructor is an addition to the
  already familiar constructor `->` used for typing functional
  expressions.

- Rules within match-expression can also contain the
  `nab X in rule` binder: in the scope of this binder, the
  symbol `X` can match existing nominals introduced by the
  `new` binder and the `\` operator.  Note that `X` is
    bound over the entire rule (including both the left and right-side
    of the rule).

## How it works
Our protoype interpreter for MLTS is written in λProlog. When you press the "Run" button the MLTS concrete syntax is transpilled to it's λProlog counterpart and [Elpi](https://github.com/LPCIC/elpi), an embeddable λProlog interpreter written in OCaml, runs it.

All this happens locally in your browser thanks to the js_of_ocaml [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml).

## Build dependencies
[TODO : does not work as-is on a fresh install] Before attempting to build the project you should make sure that you have the following dependencies installed :

- [Opam](https://opam.ocaml.org/doc/Install.html){target="_blank"} : 
```bash
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin
```
- [OCaml](https://caml.inria.fr/){target="_blank"} 4.06.0 :
```bash
opam switch install 4.06.0
```
- [Menhir](http://gallium.inria.fr/~fpottier/menhir/){target="_blank"}, [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml){target="_blank"}, [base64](https://github.com/mirage/ocaml-base64){target="_blank"} :
```bash
opam install menhir js_of_ocaml js_of_ocaml-ppx base64
```
- [Elpi](https://github.com/LPCIC/elpi){target="_blank"} :
```bash
opam pin add elpi https://github.com/LPCIC/elpi.git
```

## Credits
The "backend" is powered by OCaml, λProlog, Elpi and js_of_ocaml.

The frontend uses a standard mix of Bootsrap and Jquery but also the Ace code editor, highlight-js for additionnal syntactic coloring and open-iconic for icons.

