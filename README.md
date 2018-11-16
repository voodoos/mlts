# TryMLTS
A web application designed to try and play with the MLTS programming language.

## Sources
The source is hosted on [GitHub](https://github.com/voodoos/mlts){target="_blank"} as is the [demo](http://voodoos.github.io/mlts/){target="_blank"}.

Our prototype interpreter is written in λProlog. Its source is located in the [lib/data/core folder](https://github.com/voodoos/mlts/tree/master/lib/data/core){target="_blank"}. The more interesting parts are:

- The [type checking algorithm](https://github.com/voodoos/mlts/blob/master/lib/data/core/typing.elpi){target="_blank"} 
- The [evaluation algorithm](https://github.com/voodoos/mlts/blob/master/lib/data/core/deprime.elpi){target="_blank"}

## Basic usage
You can write code in the left panel and have it evaluated by clicking the `Run` button or the key combination `ctrl+b` while the focus is on the editor. If something goes wrong you can stop the execution by clicking `Restart kernel`.

When running your code will first be translated to the λProlog abstract syntax. Some static checking happen during this phase.
The translation's result (be it an actual translation or an error) will quickly appear on the right, under the `λProlog` tab.

Then the evaluation will start. Result will be shown in the result tab only at the very end of the process. You can witness 
progress and errors in the `Log` tab. When results are shown you can click them to quickly locate their position in the source code.

## Concrete syntax
MLTS' concrete syntax is based on the on of OCaml. A program written in MLTS not using the new constructs `nab in`, `new in`, `\` and `=>` should compile with the `ocamlc` compiler.

- Datatypes can be extended to contain new *nominal*
  constants and the `new X in body` program phrase provides a
  binding that declares that the nominal `X` is new within the
  lexical scope given by `body`.

- A new typing constructor `=>` is used to type bindings
  within term structures.  This constructor is an addition to the
  already familiar constructor `->` used to type functional
  expressions.

- The backslash `\` is an infix symbol that is used to form an abstraction of a nominal
  over its scope.  For example, `X\ body` is a syntactic
  expression that hides the nominal `X` in the scope `body`.
  Thus the backslash *introduces* an abstraction. 
  
- The `@ ` *eliminates* an abstraction for example, the
  expression `(X\body) @ Y` denotes the result of substituting
  the abstracted nominal `X` with the nominal `Y` in
  `body`.

- Rules within match-expression can also contain the
  `nab X in rule` binder: in the scope of this binder, the
  symbol `X` can match existing nominals introduced by the
  `new` binder and the `\` operator. `X` is then
    bound over the entire rule (including both the left and right-side
    of the rule).


## Known issues and missing features
This work is highly experimental and you should expect bugs to appear. If so please report them on the [issue tracker](https://github.com/voodoos/mlts/issues){target="_blank"}.

- Error messages are not always meaningful of even shown.
- Mutual recursion is coming soon.
- Pairs must always be written using parenthesis: `(a, b)` is a pair, `a, b` is not.
- There is no syntactic sugar for non-empty lists. Use `a::b::c::[]` instead of `[a;b;c]`.


## How it works
Our prototype interpreter for MLTS is written in λProlog. When you press the "Run" button the MLTS concrete syntax is transpilled to it's λProlog counterpart and [Elpi](https://github.com/LPCIC/elpi), an embeddable λProlog interpreter written in OCaml, runs it.

All this happens locally in your browser thanks to the js_of_ocaml [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml).

## Build dependencies
Ocaml modules dependency(install them via `opam`) :  `menhir elpi js_of_ocaml js_of_ocaml-ppx base64`

## Credits
The "backend" is powered by OCaml, λProlog, Elpi and js_of_ocaml.

The frontend uses a standard mix of Bootsrap and Jquery but also the Ace code editor, highlight-js for additional syntactic coloring and open-iconic for icons.

