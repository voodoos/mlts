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
let make_int i =
  App(Global("i"),
        [Lit(Int(i))])

let make_bool b =
  App(Global("b"),
        [Lit(Bool(b))])

let make_string s =
  App(Global("s"),
      [Lit(String(s))])

let make_simple_app n =
  App(Global(n), [])
  
let make_spec s args =
  App(Global("special"),
      [make_simple_app s;
      List(args)])
