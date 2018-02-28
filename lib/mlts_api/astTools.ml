open MltsAst



let const_to_string = function
  | Int(i) -> string_of_int i
  | Bool(b) -> string_of_bool b
  | EmptyList ->  "[]"
               

let infix_to_string = function
  | Mult -> "*" | Neq -> "!="
  | Lt -> "<" | Le -> "<="
  | And -> "&&" | Or -> "||"
  | Add -> "+" | Minus -> "-"
  | Equal -> "="
  | ListCons -> "::"
       
(* env -> string *)

let rec string_of_env = function
    [] -> ""
  | Global(v)::tl -> "Global(" ^ v ^ "), " ^ (string_of_env tl)
  | Local(v)::tl -> "Local(" ^ v ^ "), " ^ (string_of_env tl)
  | Param(v)::tl -> "Param(" ^ v ^ "), " ^ (string_of_env tl)
  | Nominal(v)::tl -> "Nominal(" ^ v ^ "), " ^ (string_of_env tl)
  | MetaParam(v, a)::tl -> "MetaParam(" ^ v ^ ", "^(string_of_int a)^" ), " ^ (string_of_env tl)
  
let print_env env =
  print_string "Env : "; print_string  ( string_of_env env)


(* prog -> string *)

let toString =
  let rec aux = function
      [] -> ""
    | IDef(def)::tl -> (aux_def def) ^ ";; \n" ^ (aux tl)
    | IExpr(e)::tl -> (aux_expr e) ^ ";; \n" ^ (aux tl)

  and aux_def = function
    | DLet(lb) -> "let " ^ (aux_lb lb)
    | DLetrec(lb) -> "let rec " ^ (aux_lb lb)
    | DType(n, tl) -> "type " ^ n ^ " = "
                      ^ (List.fold_left (fun acc c -> acc ^ (aux_constrs c)) "" tl)

  and aux_constrs = function
    | Simple(n) -> "\n " ^ n
    | Of(n, (typ, a)) -> "\n| "^ n ^ " of " ^ (aux_typ typ) ^ " [" ^ (string_of_int a) ^ "]"

  and aux_typ = function
    | Cons(n) -> n
    | Sum((t1, a1), (t2, a2)) -> (aux_typ t1) ^ "("^(string_of_int a1)^") * " ^(aux_typ t2) ^ "("^(string_of_int a2)^")"
    | Arrow((t1, a1), (t2, a2)) -> "(" ^ (aux_typ t1) ^ "("^(string_of_int a2)^")" ^" -> " ^(aux_typ t2) ^ "("^(string_of_int a2)^")" ^ ")"
                                          
  and aux_lb = function
    | LBVal(name, pl, expr) -> name
                           ^ (List.fold_left (fun acc p -> acc ^ " " ^ p) "" pl)
                           ^ " = " ^ (aux_expr expr)

  and aux_expr = function
    | ELetin(lb, e) -> "let " ^ (aux_lb lb) ^ " in\n\t" ^ (aux_expr e)
    | ELetRecin(lb, e) -> "let rec " ^ (aux_lb lb) ^ " in\n\t" ^ (aux_expr e)
    | EApp(e, el) -> "EApp:(" ^ (aux_expr e) ^ ")" 
                     ^ (List.fold_left (fun acc e -> acc ^ " ("
                                                     ^ (aux_expr e) ^ ")"
                                       ) "" el)
    | EIf(e1, e2, e3) -> "if (" ^ (aux_expr e1) ^ ") "
                              ^ "\n then (" ^ (aux_expr e2) ^ ") "
                              ^ "\n else (" ^ (aux_expr e3) ^ ") "
    | EMatch(e, pm) -> "match " ^ (aux_expr e)
                       ^ " with " ^ (aux_pm pm)
    | EPair(e1, e2) -> "EPair: (" ^ (aux_expr e1) ^ ", "^ (aux_expr e2) ^") "
    | EInfix(e1, op, e2) -> (aux_expr e1) ^ (infix_to_string op) ^ (aux_expr e2)
    | EConst(c) -> const_to_string c
    | EVal(vp) -> vp
    | EConstr(n, l) -> n ^ "(" ^ (List.fold_left
                                    (fun acc p -> acc ^( aux_expr p)^",")
                                    "" l) ^ ")"
    | EPattern(p) -> aux_pattern p
    | EBind(v, e) -> "("^v^"\\ ("^(aux_expr e)^"))"
    | EFun(v, e) -> "(fun "^v^"\\ ("^(aux_expr e)^"))"
    | ENew(v, e) -> "(new "^v^"\\ ("^(aux_expr e)^"))"

  and aux_pm pm =
    List.fold_left (fun acc rule ->
        match rule with
        | RSimple(p, e) -> acc ^ "\n| "
                                      ^ (aux_pattern p)
                                      ^ " -> " ^ (aux_expr e)
        | RNa(n, p, e) -> acc ^ "\n| nab " ^(List.fold_left (fun acc s -> acc ^ " " ^ s) "" n)^"\\ ("
                                      ^ (aux_pattern p)
                                      ^ " -> " ^ (aux_expr e)^ ")")
                   ""
                   pm

  and aux_pattern = function
    | PVal(v) -> v
    | PBind(v, p) -> v ^ "\\ " ^ (aux_pattern p)
    | PApp(v, l) -> "PApp:"^v ^ "(" ^ (List.fold_left
                                    (fun acc p -> acc ^( aux_pattern p)^",")
                                    "" l) ^ ")"
    | PConstr(p, l) -> p ^ "(" ^ (List.fold_left
                                    (fun acc p -> acc ^( aux_pattern p)^",")
                                    "" l) ^ ")"
    | PConstant(c) -> const_to_string c
    | PListCons(p1, p2) -> (aux_pattern p1) ^ "::" ^ (aux_pattern p2)
    | PPair(p1, p2) -> "PPair: (" ^ (aux_pattern p1) ^ "," ^ (aux_pattern p2) ^ ")"
  in aux
