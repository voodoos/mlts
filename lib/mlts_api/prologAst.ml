type global_name = string
type local_name = string * int
type name = 
| Local of local_name
| Global of global_name

type ty = 
| Name of global_name
| Arrow of ty * ty

type term = 
| Seq of term list
| Abs of local_name * term
| Hyp of term * term
| Eq of term * term
| App of atom * term list
| Cons of term * term
| List of term list
| Lit of literal

and literal =
  | Int of int
  | String of string
  | Bool of bool

and atom = name

type decl = { 
    sort: decl_sort; 
    name: global_name; 
    ty: ty; 
}
and decl_sort = Kind | Type

type def = {
    name: global_name;
    args: term list;
    body: term option;
}

type clause =
| Declaration of decl
| Definition of def

type prog = clause list



(* TOOLS *)
let make_prog name body =
  App(Global("prog"),
      [Lit(String(name));
       body])
  
let make_int i =
  App(Global("i"),
      [Lit(Int(i))])

let make_bool b =
  App(Global("b"),
        [Lit(Bool(b))])

let make_string s =
  App(Global("s"),
      [Lit(String(s))])

let make_global n =
  App(Global(n), [])

let make_local n i =
  App(Local(n, i), [])
  
let make_spec s args =
  App(Global("special"),
      [make_global s;
      List(args)])

let make_lam a1 a2 lvar inner =
  App(Global("lam"),
      [make_local "todo_ar" a1;
       make_local "todo_ar" a2;
      Abs(lvar, inner)   
      ])

let make_let a1 a2 lvar inner =
  App(Global("let"),
      [make_local "todo_ar" a1;
       make_local "todo_ar" a2;
      Abs(lvar, inner)   
      ])

    
let make_app a1 a2 f args =
  App(Global("app"),
      [make_local "todo_ar" a1;
       make_local "todo_ar" a2;
       f;
       List args;
    ])

let make_deps fvs = 
  let make_dep name =
    make_prog name (make_global name)
  in match fvs with
     | [] -> None
     | _ -> Some(Seq (List.map make_dep fvs))

