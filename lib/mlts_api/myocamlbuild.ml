open Ocamlbuild_plugin

module Menhir = struct
  let menhir () =
    if !Options.ocamlyacc = N then V"MENHIR" else !Options.ocamlyacc
  let menhir_tags mly =
    tags_of_pathname mly ++"ocaml"++"parser"++"menhir"

  let menhir_produce_messages env build =
    let messages, mly = env "%.messages", env "%.mly" in
    let open Ocamlbuild_pack in
    Ocaml_compiler.prepare_compile build mly;
    Cmd(S[menhir (); T (menhir_tags mly);
          A "--list-errors"; P mly; Sh ">"; Px messages])

  let menhir_compile_messages env build =
    let mly = env "%.mly" in
    let messages = env "%.messages" in
    let target = env "%_messages.ml" in
    Cmd(S[menhir (); T (menhir_tags mly); P mly;
          A "--compile-errors"; P messages;
          Sh ">"; Px target])

  let menhir_update_messages env build =
    let mly = env "%.mly" in
    let messages = env "%.messages" in
    let tmp = Filename.temp_file "menhir" ".messages" in
    Seq [
      Cmd(S[menhir (); T (menhir_tags mly); P mly;
            A "--update-errors"; P messages;
            Sh ">"; P tmp]);
      Cmd(S[A "mv"; P tmp; P messages]);
    ]

  let dispatcher = function
      | After_rules ->
        flag ["menhir"; "parser"; "menhir_trace"] (A"--trace");
        flag ["menhir"; "parser"; "menhir_table"] (A "--table");
        flag ["menhir"; "parser"; "menhir_canonical"] (A"--canonical");
        rule "menhir: .mly -> .messages"
          ~prod:"%.messages"
          ~deps:["%.mly"]
          menhir_produce_messages;
        rule "menhir: .mly & .messages -> _messages.ml"
          ~prod:"%_messages.ml"
          ~deps:["%.mly"; "%.messages"]
          menhir_compile_messages;
        rule "menhir: .mly & .messages -> .messages & .messages.update"
          ~stamp:"%.messages.update"
          ~deps:["%.mly"; "%.messages"]
          menhir_update_messages;
      | _ -> ()
end

(* Howto: create and maintain parser error-message files

# If you have no .messages file in your repository,
# create one from foo.mly with the following commands:

ocamlbuild -use-ocamlfind foo.messages
cp _build/foo.messages .

# Once you have a foo.messages file, you can get
# an OCaml module Foo_messages with the following command

ocamlbuild -use-ocamlfind foo_messages.ml

# (see _build/foo_messages.ml for what this
#  auto-generated source file looks like.)
# You don't actually need to run this command and can use Foo_messages
# as an OCaml module in your code, ocamlbuild will build it on-demand

# Once you have your foo.messages file, you need to fill the error messages
# in it. foo_messages.ml will get updated automatically. However, if your
# foo.mly file changes, foo.messages will not get updated automatically
# (this requires manual error-message adaptation in general),
# so you should manually request the update with using the
# foo.messages.update stamp target:

ocamlbuild -use-ocamlfind foo.messages.update
cp _build/foo.messages .

# then check using your version-control system that the changes
# to the .messages file are as you expect

*)


let _ =
       dispatch
         (fun hook ->
            Menhir.dispatcher hook;
         )


