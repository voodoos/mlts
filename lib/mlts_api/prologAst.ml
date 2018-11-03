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

let make_app name tms =
  App(Global name, tms)

let make_lit l =
  make_app "lit" [l]

let make_int i =
  make_lit (make_app "i" [Lit (Int i)])

let make_bool b =
  make_lit (make_app "b" [Lit (Bool b)])

let make_string s =
  make_lit (make_app "s" [Lit (String s)])

let make_global n =
  make_app n []

let make_local n i =
  App(Local(n, i), [])
  
let make_spec s args =
  App(Global("special"),
      [make_global s;
      List(args)])

let make_lam _a1 _a2 lvar inner =
  App(Global("lam"),
      [Abs(lvar, inner)])

let make_let _a1 _a2 lvar inner =
  App(Global("let"),
      [Abs(lvar, inner)])

    
let make_appt f args =
  make_app "app" (f::(args))
    
let make_prog name body =
  App(Global("prog"),
      [Lit(String(name));
       body])
  
let make_deps fvs = 
  let make_dep name =
    make_prog name (make_global (String.capitalize_ascii name))
  in match fvs with
     | [] -> None
     | _ -> Some(Seq (List.map make_dep fvs))

