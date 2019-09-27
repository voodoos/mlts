open PrologAst

(* AST for MLTS programs written
uising the lProlog abstract syntax *)
let local_name = ("toto", 2)
let global_name = "tata"

let prop = Name "prop"
let tm = Name "tm"

let (@->) ty1 ty2 = Arrow (ty1, ty2)

let ty =
  let tm = Name "tm" in
  (tm @-> tm) @-> tm @-> tm

let decl_tm = {
  sort = Kind;
  name = "tm";
  ty = Name "type";
}

let decl_app = {
  sort = Type;
  name = "app";
  ty = tm @-> tm @-> tm;
}

let decl_lam = {
  sort = Type;
  name = "lam";
  ty = (tm @-> tm) @-> tm;
}

let decl_beta = {
  sort = Type;
  name = "beta";
  ty = tm @-> tm @-> prop;
}

let local str = App (Local (str, 0), [])
let app t u = App (Global "app", [t; u])
let lam t  = App (Global "lam", [t])

let def_beta_1 = {
  name = "beta";
  args = [
    lam (local "R");
    lam (local "R");
  ];
  body = None;
}
let def_beta_2 = {
  name = "beta";
  args = [
    app (lam (local "R")) (local "U");
    local "V";
  ];
  body = Some (
      App (Global "beta",
           [App (Local ("R", 0), [local "U"]);
            local "V"])
    );
}

let prog = [
  Declaration decl_tm;
  Declaration decl_app;
  Declaration decl_lam;
  Declaration decl_beta;
  Definition def_beta_1;
  Definition def_beta_2;
]

let () =
  let open Format in
  printf "%a@."
    PrologPrinter.pp_prog prog

(*
beta (lam R) (lam R).
beta (app (lam R) U) V :- beta (R U) V.
*)
