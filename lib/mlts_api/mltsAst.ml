type prog = module_item list

and module_item =
  | IDef of definition * Lexing.position
  | IExpr of expr * Lexing.position

and definition =
  | DLet of let_binding
  | DLetrec of let_binding list
  | DType of typeconstr_name * (constr_decl list)

and constr_decl =
  | Simple of constr_path
  | Of of constr_path * typexpr

and let_binding =
  | LBVal of value_name * param list * expr

and expr =
  | ELetin of let_binding * expr
  | ELetRecin of let_binding * expr
  | EMatch of expr * pattern_matching
  | EIf of expr * expr * expr
  | EApp of expr * (argument list)
  | EBApp of expr * nominal list
  | EInfix of expr * infix_op * expr
  | EConst of constant
  | EVal of value_path
  | EPair of expr * expr
  | EConstr of  constr_path * (expr list)
  | EPattern of pattern
  | EBind of nominal * expr
  | EFun of (value_name list) * expr
  | ENew of nominal * expr


and typexpr =
  | Cons of constr_path
  | Pair of typexpr * typexpr
  | Sum of typexpr * typexpr
  | Arrow of typexpr * typexpr
  | Bind of typexpr * typexpr
  | List of typexpr

and pattern_matching = rule list

and rule =
  | RSimple of pattern * expr
  | RNa of (value_name list) * pattern * expr

and pattern =
  | PVal of value_name
  | PBind of value_name * pattern
  | PApp of value_name * (pattern list)
  | PBApp of value_name * nominal list
  | PConstr of  constr_path * (pattern list)
  | PConstant of constant
  | PListCons of pattern * pattern
  | PPair of pattern * pattern
  | PAny of value_name

and constant =
  | Int of int
  | Bool of bool
  | String of string
  | Unit
  | EmptyList

and infix_op =
  | Add | Minus | Mult | Neq
  | Equal | Lt | Le | And | Or
  | ListCons | Seq

and arity = int
and param = value_name
and argument = expr
and typeconstr_name = string
and constr_name = string
and constr_path = constr_name
and value_name = string
and value_path = value_name
and nominal = value_name

type var =
    Global of value_name
  | Local of value_name
  | Param of value_name
  | Nominal of nominal
  | MetaParam of value_name * int

type env = (var * arity) list









