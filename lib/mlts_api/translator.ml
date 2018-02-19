open MltsAst

let to_separated_list ?first:(f = false) s l =
  let rec aux = function
    | [] -> ""
    | [x] -> "(" ^ x ^ ")"
    | x::tl -> "(" ^ x ^ ")" ^ s ^ (aux tl)
  in
  if f && l != [] then s ^ (aux l) else (aux l)
  
let firsts l = List.map (fst) l
let seconds l = List.map (snd) l


(* Utilities to construct strings of LP *)

let specials = 
  let h = Hashtbl.create 17 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
            [ "List.hd", "car";
              "List.tl", "cdr";
              "App", "app";
              "Appv", "ap";
              "Abt", "abt";
              "Abtv", "ab"] ;
  fun s ->
  try  Hashtbl.find h s
  with Not_found -> s
                             
let const_to_lpstring = function
  | Int(i) -> "(i " ^ (string_of_int i) ^ ")"
  | Bool(true) -> "tt"
  | Bool(false) -> "ff"
  | EmptyList ->  "null"
                              
let infix_to_lpstring = function
  | Mult -> "times" | Neq -> "nequal"
  | Lt -> "lt" | Le -> "le"
  | And -> "and" | Or -> "or"
  | Add -> "sum" | Minus -> "minus"
  | Equal -> "equal"
  | ListCons -> "cons"

let prog_string name code freevars =
  if code <> "" then (
  "prog \"" ^ name ^ "\""
  ^ " (" ^ code ^ ")"
  ^ ( let sizeEnv = List.length freevars in
      if sizeEnv > 0 then begin
          let count = ref sizeEnv in
          " :- "
          ^ List.fold_left (fun acc s ->
              count := !count - 1;
              acc ^ "prog \"" ^ s ^ "\" " ^ (*s ^ ", "
              ^ "eval P" ^ s ^ " " ^*) String.capitalize_ascii s
              ^ (if !count > 0 then ", " else ""))
                         ""
                         freevars
        end
      else "")
  ^ "." 
  (*^ " % " ^ ( to_separated_list ", " freevars)*)
  ^ "\n\n" ) else ""

let let_binding_val name args code =
  name ^ "\\"
  ^  (List.fold_left (fun acc p -> acc ^ "lam " ^ p ^ "\\ ") " " args)
  ^ code
      
      
let func name args =
  name ^ "\\"
  ^  (List.fold_left (fun acc p -> acc ^ "lam " ^ p ^ "\\ ") " " args)
      
let fixpoint name args code =
  "(fixpt " ^ func name args ^ code ^ ")"

let letin name args code body =
  "(let (" ^ code ^ ") (" ^ func name args ^ " " ^ body ^ "))"

                                                            
let letrecin name args code body =
  "(let (fixpt " ^ func name args ^ " (" ^ code ^ ")) (" ^ name ^ "\\ (" ^ body ^ ")))"
      
let app head args =
  (specials head)
  (*^"toto"^(string_of_int (List.length args))*)
  ^ (to_separated_list ~first:true " arobase " args)
      
let appv head args =
  (specials head) 
  ^ (to_separated_list ~first:true " " args)

let if_then_else cond e1 e2 =
  "cond (" ^ cond ^ ") "
  ^ "\n (" ^ e1 ^ ") "
  ^ "\n (" ^ e2 ^ ") " 

let binop op e1 e2 =
  (infix_to_lpstring op) ^ " arobase (" ^ e1 ^ ") arobase (" ^ e2 ^ ")"

let list_of_rules rules =
  "\n["
  ^ List.fold_left (fun acc r -> acc ^ ", \n" ^ r) (List.hd rules) (List.tl rules)
  ^ "]"


let matcher_arrow pararities pattern expr =
  "("
  ^ (List.fold_left (fun acc (p, a) -> 
		acc 
		^ (match a with
			0 -> "all "
			| 1 -> "all' "
			| 2 -> "all'' "
			| _ -> "ALL? " )
		^ (String.capitalize_ascii p) ^ "\\ ") "" pararities)
  ^ " ((" ^ pattern ^ ") ==> (" ^ expr ^ ")))"
  
let matcher_na name pararities pattern expr =
"(nab "^name^"\\ ("
^ matcher_arrow pararities pattern expr
^ "))"

let bind v  e =
  "(" ^ v ^ "\\ (" ^ e ^ "))" 
  
let func v  e =
  "(lam " ^ v ^ "\\ (" ^ e ^ "))" 
  
let newc v  e =
  "(new " ^ (String.uncapitalize_ascii v) ^ "\\ (" ^ e ^ "))" 

                                          
let type_constr name l =
  name ^ " " ^ (to_separated_list " " l)

                 

(* 	Type constructors handling *)
let rec arity_list_of_type_term = function
	| (Cons(_), a) -> [a]
	| (Arrow(_,_), a) -> [a]
	| (Sum((_, a1), ate2), _) -> a1::(arity_list_of_type_term ate2)

let add_constr constructors = function
	| Of(n, ate) ->
	   let aritylist = arity_list_of_type_term ate in
	   Hashtbl.add constructors n aritylist
        | Simple(n) ->
           Hashtbl.add constructors n []

let remove_constr constructors = function
  | Simple(n) | Of(n, _) -> Hashtbl.remove constructors n


(* PROGRAM TRANSLATION *)
let toLPString p =
  let c = ref 0 in
  let constructors = Hashtbl.create 100 in

    

  
  let rec aux env = function
      [] -> ("", [], [])
    | item::tl ->
       let name, str, freevars, datatypes =
         (match item with
          | IDef(def) ->
             let name = getDefName def in
             let str, freevars, datatypes = aux_def env def in
             name, str, freevars, datatypes
        | IExpr(e)  -> 
           let a = c := !c + 1; !c in
           let name = "test_" ^ (string_of_int a) in
           let str, freevars = aux_expr ((Local name)::env) e in
           name, str, freevars, []
         ) in
       let strNext, freevars2, datatypes2 = aux ((Global name)::env) tl in
       (prog_string name str freevars ^ strNext), freevars2, datatypes @ datatypes2

  and aux_def env = function
    | DLetrec(lb) ->
       let name, params, code, freevars = aux_lb env lb in
       fixpoint name params code, freevars, []
    | DType(name, clist) -> 
		(* When encountering a type declaration, we add the type constructors to 
		the constructors Hashtbl with the arguments arities as a list *)
		List.iter (add_constr constructors) clist;
		"", [], []

  and aux_lb env = function
    | LBVal(name, pl, expr) ->
       let str, freevars = aux_expr ((Local name)::(params_to_env env pl)) expr in
       name, pl, str, freevars
       
  and aux_expr env = function
    | ELetin(lb, e) ->
       let name, params, code, freevars = aux_lb env lb in
       let body, body_fv = aux_expr ((Local name)::env) e in
       letin name params code body, freevars @ body_fv
    | ELetRecin(lb, e) ->
       let name, params, code, freevars = aux_lb env lb in
       let body, body_fv = aux_expr ((Local name)::env) e in
       letrecin name params code body, freevars @ body_fv
    | EApp(e, el) ->
       let str, freevars = (aux_expr env e) in
       let args = List.map (aux_expr env) el in
	   let a = arityMP (String.uncapitalize_ascii str) env in
	  (*  "["^(string_of_env env) ^ 
       "]["^str ^ " has arity " ^(string_of_int a)^"]" 
	   ^*)
	   (*"["^str ^ " has arity " ^(string_of_int a)^"]")^*)(if a >= 1 then
	   (appv str (List.map (fst) args))
	   else 
		(app str (List.map (fst) args))
		), List.fold_left (fun acc (_, fv) -> acc@fv) freevars args
    | EIf(e1, e2, e3) ->
       let str1, fv1 = aux_expr env e1
       and str2, fv2 = aux_expr env e2
       and str3, fv3 = aux_expr env e3
       in
       if_then_else str1 str2 str3, fv1 @ fv2 @ fv3
    | EMatch(e, pm) ->
       let code, freevars = aux_expr env e in
       let rules = List.map (aux_rule env) pm in
       let rulesTxt = List.map (fst) rules in
       let rulesFreevars =
         List.fold_left (fun acc (_, fv) -> fv@acc) [] rules in
       "match (" ^ code ^ ") " ^ (list_of_rules rulesTxt), freevars @ rulesFreevars
    | EInfix(e1, op, e2) ->
       let str1, fv1 = aux_expr env e1
       and str2, fv2 = aux_expr env e2
       in
       binop op str1 str2, fv1 @ fv2
    | EConst(c) -> const_to_lpstring c, []
    | EVal(vp) -> 
       if (List.mem (Global vp) env) then
         (String.capitalize_ascii vp), [vp]
       else 
	 begin 
	   let a = arityMP vp env in
	   if a >= 0
           then (String.capitalize_ascii vp), []
           else vp, []
	 end
    | EConstr(n, elist) ->
		(try	
			let constr = Hashtbl.find constructors n in
			let n = String.uncapitalize_ascii n in
			
			(*print_string ("\nFound : " ^n^ "\n");
			List.iter (print_int) constr;*)
			
			let exprs = List.map (aux_expr env) elist in
			let codes = List.map (fst) exprs in
			let fvs = List.flatten (seconds exprs) in
			
			if (List.fold_left (+) 0 constr) > 0 then
				(appv n codes), fvs
			else
				(app n codes) , fvs
		with Not_found -> failwith ("Unknown constructor " ^ n))
    | EPattern(p) -> let code, pararities = aux_pattern env p in
						code, firsts pararities
    | EBind(v, e) -> let code, fv = aux_expr (Local(v)::env) e in
                     bind v code,fv 
    | EFun(v, e) -> let code, fv = aux_expr (Local(v)::env) e in
                     func v code,fv 
    | ENew(v, e) ->
       add_constr constructors (Simple(v));
       let code, fv =  aux_expr env e in
       remove_constr constructors (Simple(v));
       newc v code,fv 
                             
  and aux_rule env = function
    | RSimple(p, e) ->
        let pattern, pararity = aux_pattern env p in
        let expr, freevars = aux_expr (metaparams_to_env env pararity) e in
        matcher_arrow pararity pattern expr, freevars
                                               
    | RNa(n, p, e) ->
        add_constr constructors (Simple(n));
        let pattern, pararity = aux_pattern env p in
        let expr, freevars = aux_expr (metaparams_to_env env pararity) e in
        remove_constr constructors (Simple(n));
        matcher_na (String.uncapitalize_ascii n) pararity pattern expr, freevars
                                                
  and aux_pattern env = function
    | PVal(vn) ->  
       if (List.mem (Nominal(vn)) env) then vn, []
       else if (Hashtbl.mem constructors vn) then vn, []
       else (String.capitalize_ascii vn), [(vn, 0)]
    | PApp(vn, pls) ->
       let pl = List.map (aux_pattern env) pls in
       let vn = String.capitalize_ascii vn in
       print_endline ("Found " ^ vn ^ " ( " ^ (string_of_int (List.length pls))^ " )");
       vn ^ (to_separated_list ~first:true " " (firsts pl)) , [vn, List.length pls]
    | PConstr(cp, pls) ->
		(try
		   let arities = Hashtbl.find constructors cp in
		   let name = if arities = []
                              then String.uncapitalize_ascii cp
                              else specials (cp ^ "v")
                   in
		   let pl = List.map (aux_pattern env) pls in
		   (* TODO CONSTRS IN CONSTRS MAY FAIL *)
		   let params = firsts (List.flatten (seconds pl)) in
                   let parities = seconds (List.flatten (seconds pl)) in
		   let marities = List.map2 (fun l1 l2 -> max l1 l2) parities arities in
                   let pararity = List.map2 (fun l1 l2 -> l1, l2) params marities in
                   (* let pararity = (List.flatten (seconds pl)) in *)
		   type_constr name (firsts pl), pararity
		 with Not_found -> failwith ("Constructor unknown in pattern : "^cp))
    | PConstant(c) -> const_to_lpstring c, []
    | PListCons(p1, p2) -> 
       let p1, fv1 = aux_pattern env p1
       and p2, fv2 = aux_pattern env p2 in
       "cns (" ^ p1 ^ ") ("  ^ p2 ^ ")", fv1 @ fv2

  in

  let progs, _, types = aux [] p in
     (*let types, typing, eval =  typesToLPString types in*)
     progs, "", "",  "" (* types, typing, eval*)



                     
let make_lp_file prog =
  let progmod = open_out "export/progs_gen.mod" in
  let dtmod = open_out "export/datatypes.mod" in
  let dtsig = open_out "export/datatypes.sig" in
  
  let progs,  types, typing, eval = toLPString prog in

  (* progs_gen.mod *)
  output_string progmod "module progs_gen.\n\n";
  output_string progmod progs;

  (* datatypes.mod 
  output_string dtmod "module datatypes.\n\n";
  output_string dtmod typing;
  output_string dtmod eval; *)


  (* datatypes.sig
  output_string dtsig "sig datatypes.\naccum_sig eval.
                       accum_sig typing.\n\n";
  output_string dtsig types; *)

  
  close_out progmod;
  close_out dtmod;
  close_out dtsig
            
