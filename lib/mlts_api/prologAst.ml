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

let make_lit is_pat l =
  make_app (if is_pat then "plit" else "lit") [l]

let make_int ?(is_pat = false) i =
  make_lit is_pat (make_app "i" [Lit (Int i)])

let make_bool ?(is_pat = false) b =
  make_lit is_pat (make_app "b" [Lit (Bool b)])

let make_string ?(is_pat = false) s =
  make_lit is_pat (make_app "s" [Lit (String s)])

let make_global n =
  make_app n []

let make_local n i =
  App(Local(n, i), [])

let make_localp p = make_local (fst p) (snd p)
  
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
    
let make_nom_appt f args =
  make_app "arobase" (f::(args))
  
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

let rec make_rule nabs vars pat body =
  match nabs, vars with
  | n::ntl, _ -> make_app "nab" [Abs(n, make_rule ntl vars pat body)]
  | [], v::vtl -> make_app "all" [Abs(v, make_rule nabs vtl pat body)]
  | [], [] -> make_app "arr" [pat; body]

let make_match tm rules =
  make_app "match" [tm; List rules]

let make_pvar name id =
  make_app "pvar" [make_local name id]

let make_pnom name id =
  make_app "pnom" [make_local name id]
    
let make_nom name id =
  make_app "nom" [make_local name id]

let make_ite tm1 tm2 tm3 =
  make_app "if_then_else" [tm1; tm2; tm3]

let make_new name tm =
  make_app "new" [Abs(name, tm)]

let make_bind name tm =
  make_app "backslash" [Abs(name, tm)]

