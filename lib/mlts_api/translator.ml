open MltsAst

module P = PrologAst

exception TranslatorError of string * (Lexing.position option)

type def = { mutable name: string; mutable pos: Lexing.position }


let make_exception message def pos =
  TranslatorError("In \"" ^ def.name ^ "\" (line "
                  ^ (string_of_int def.pos.pos_lnum)
                  ^ ") : " ^ message,
                  match pos with
                    None -> Some(def.pos)
                  | Some(_p) -> pos)

(* Context : global vars and counters *)
type context = {
    mutable nb_expr: int;
    mutable global_vars : string list;
    mutable global_constrs : string list;
    mutable actual_def: def list;
  }
let incr_expr c = c.nb_expr <- c.nb_expr + 1

(* (local) Environments *)
type env = {
    local_vars: P.local_name list;
    free_vars:  P.global_name list;
    pattern_vars: P.local_name list;
    local_noms: P.local_name list;
  }
         
let flip f = (fun y x -> f x y)

let add_to_env v env = 
  match List.assoc_opt v env.local_vars with
  | None -> (v, 0), { env with local_vars = (v, 0)::env.local_vars }
  | Some(i) -> (v, i + 1), { env with local_vars = (v, i + 1)::env.local_vars }
             
let add_nom_to_env v env = 
  match List.assoc_opt v env.local_noms with
  | None -> (v, 0), { env with local_noms = (v, 0)::env.local_noms }
  | Some(i) -> (v, i + 1), { env with local_noms = (v, i + 1)::env.local_noms }
             
let first_in_env v env =
  try List.assoc v env.pattern_vars with
    Not_found -> List.assoc v env.local_vars

let string_of_env env =
  let string_of_var (v, i) = Printf.sprintf "(%s, %d)" v i in
  let section name to_string li = [ name; ": " ] @ List.map to_string li @ [ "\n" ] in
  String.concat "" ([]
    @ section "local_vars" string_of_var env.local_vars
    @ section "free_vars" (fun s -> s) env.free_vars
    @ section "pattern_vars" string_of_var env.pattern_vars
    @ section "local_noms" string_of_var env.local_noms
   )
let print_env env = print_endline (string_of_env env)     

(* env are used to pass is used to carry different kind of information. Some, such as local variables should be erased when leaving scope, some like the list of "free variables" needed by an expr should not.

 This may be bad design (todo ?) but for now the revert_locals function can be use to strip newly declared locals using an "Original" environement without the new locals and the  "deeper" env with maybe new locals and infos *)
let revert_locals envBefore envAfter = {
    envAfter with local_vars = envBefore.local_vars
  }
let revert_noms envBefore envAfter = {
    envAfter with local_noms = envBefore.local_noms
  }
let revert_patterns envBefore envAfter = {
    envAfter with pattern_vars = envBefore.pattern_vars
  }
                                     
let map_with_env f env li =
  let accumulate (acc, env) input =
    let (output, env) = f env input in
    (output::acc, env) in
  let rev_results, env = List.fold_left accumulate ([], env) li in
  List.rev rev_results, env
    
type k = Decl | Def
type tdef = { kind: k;  name: string; body: P.term; env: env }

(* PROGRAM TRANSLATION *)
let mlts_to_prolog p =
  let ctx = { nb_expr = 0;
              global_vars = ["list_hd"; "list_tl"];
              global_constrs = ["pair"; "list_empty"; "list_cons"];
              actual_def = []
            }
  in
  let add_global v = ctx.global_vars <- v::ctx.global_vars in
  let add_constr c = ctx.global_constrs <- c::ctx.global_constrs in
  let set_actual_name n =
    (List.hd (ctx.actual_def)).name <- n;
  in
  let set_actual_def name pos =
    ctx.actual_def <- { name; pos } :: ctx.actual_def;
  in
  

  
  let rec t_items = function
    | [] -> []
    | item::tl -> 
       let titem = t_item item in
       titem@(t_items tl)

  and t_item =
    (* Each "toplevel" item transpile to a program *)
    (* Each "toplevel" item ha its own env but shares
         the global context *)
    let env = {
        local_vars = [];
        free_vars = [];
        pattern_vars = [];
        local_noms = []
      }
    in
    function
    | IDef(def, pos) ->
       set_actual_def "" pos;
       t_def env def
    | IExpr(expr, pos) ->
       let name = "val_" ^ (string_of_int (ctx.nb_expr)) in
       set_actual_def name pos;
       incr_expr ctx; 
       let texpr, env = t_expr env expr in
       [P.Definition({
         name = "prog";
         args = [P.Lit(P.String(name)); 
                 texpr];
         (* Some programs may depend on others! *)
         body = P.make_deps (env.free_vars)
       })]

  and t_def env =
    function
    | DLet(LBVal(name, params, e)) ->
      set_actual_name name;
       let body, env = make_lam env params e in
       add_global name;
       [P.Definition({
             name = "prog";
             args = [P.Lit(P.String(name)); body];
             body = P.make_deps (env.free_vars);
       })]
    | DLetrec(LBVal(name, params, e)) ->
      set_actual_name name;
       let ln, env = add_to_env name env in
       (*let name = ((fst ln) ^ "_" ^ (string_of_int (snd ln))) in*)
       let body, env = make_lam env params e in
       add_global name;
       
       print_endline ("Debug: end freevars found: "
                      ^ String.concat "" env.free_vars);
       [P.Definition({
             name = "prog";
             args = [P.Lit(P.String(name));
                     P.make_fix ln body];
             body = P.make_deps (env.free_vars);
       })]

       
      (* let _, env2 = add_to_env name env in
       let d = t_def env2 (DLet l) in
       { d with env = revert_locals env d.env }*)
    | DType(name, decls) -> 
    set_actual_name name;
    let rec list_of_sum ty = 
      let rec t_typ = function
        | Cons(c) -> P.make_global c
        | Sum(_, _) as s -> P.List (List.rev (list_of_sum s))
        | Arrow(ty1, ty2) -> P.make_app "arr" [t_typ ty1; t_typ ty2]
        | Bind(ty1, ty2) -> P.make_app "bigarr" [t_typ ty1; t_typ ty2]
        | List(_t) -> failwith "List (type) not implemented"
      in
      match ty with
      | Sum(ty1, ty2) -> (t_typ ty2)::(list_of_sum ty1)
      | _ -> [t_typ ty]
    in
    let type_decl =
      P.Declaration({
              sort = Type;
              name =  name;
              ty = P.Name "ty"
          })
    in
    let constructors =  List.map (fun decl ->
      let c, typ = match decl with
                  | Simple(c) -> c, P.List []
                  | Of(c, typ) -> c, P.List (List.rev (list_of_sum typ))
      in
      add_constr c;
      [P.Declaration({
            sort = Type;
             name =  c;
             ty = P.Name "constructor"
          });
       P.Definition({
             name =  "type_constr";
             args = [P.make_global c;
                     typ;
                     P.make_global name];
             body = None
          });
       ]
      ) decls in
      type_decl::(List.flatten constructors)

  
      
  and t_expr envIn = function
    | ELetin(LBVal(name, params, expr), body) ->
       (* let f x y = x + y in f 2 3;; *)
       (* Add params to env for expr *)
       let _params, env =
         map_with_env (flip add_to_env) envIn params in
       let exprtm, env = t_expr env expr in
    (* Removing params from env (but keeping freevars) *)
       let env = revert_locals envIn env in
    (* Adding the let-local name to env for body *)
       let lname, env = add_to_env name env in
       let bodytm, env = t_expr env body in
    (* Removing it *)
       P.make_letin lname exprtm bodytm, revert_locals envIn env
                                                
    | ELetRecin(LBVal(name, params, e), body) ->
       (*let name = ((fst ln) ^ "_" ^ (string_of_int (snd ln))) in*)
       let ln, env = add_to_env name envIn in
       let letbody, env = make_lam env params e in
       let body, env =t_expr env body in
       P.make_letrecin ln letbody body, revert_locals envIn env
                       
    | EMatch(e, rules) ->
       let e, env = t_expr envIn e in
       let rules, env = map_with_env t_rule env rules in
       P.make_match e rules, env
       
    | EIf(e1, e2, e3) ->
       let tm1, env = t_expr envIn e1 in
       let tm2, env = t_expr env e2 in
       let tm3, env = t_expr env e3 in
       P.make_ite tm1 tm2 tm3, env
       
    | EApp(e, args) -> 
       let te, env = t_expr envIn e in
       let args, env = map_with_env t_expr env args in
       P.make_appt te args, env
       
    | EBApp(e, args) ->
       let te, env = t_expr envIn e in
       let args, env = map_with_env t_expr env args in
       P.make_nom_appt te args, env
                        
    | EInfix(e1, op, e2) -> 
       let te1, env = t_expr envIn e1 in
       let te2, env = t_expr env e2 in
       P.make_spec op [te1; te2],
       env
       
    | EConst(c) -> t_constant c, envIn
                 
    | EVal(v) -> 
       begin
         print_env envIn;
         try P.make_local v (first_in_env v envIn), envIn
         with Not_found -> (* Non-local must be global *)
           if (List.mem v ctx.global_vars) then
             let v2 = String.capitalize_ascii v in
             let env = if List.mem v envIn.free_vars then envIn
                       else { envIn with free_vars = v::envIn.free_vars }
             in P.make_global v2, env
           else raise (make_exception
               ("Unbound value '" ^ v ^ "'.")
               (List.hd ctx.actual_def)
               None)
       end
    | EPair(e1, e2) ->
       t_expr envIn (EConstr("pair", [e1; e2]))
                   
    | EConstr(name, exprs) -> 
       (* A constructor is either 
          a global datatype constructor 
         or a localnominal *)
       begin
         try
           let i = List.assoc name envIn.local_noms in
           if exprs = [] then
             P.make_nom name i, envIn
           else raise (make_exception
               ("Nominals cannot have arguments. (" ^ name ^ ")")
               (List.hd ctx.actual_def)
               None)
         with
           Not_found ->
           if List.mem name ctx.global_constrs then
             let tms, env = map_with_env t_expr envIn exprs in
             P.make_constr name tms, env
           else
             raise (make_exception
               ("Unknown constructor " ^ name)
               (List.hd ctx.actual_def)
               None)
               
       end
    | EPattern(_) -> failwith "Not implemented: EPattern"
                   
    | EBind(name, body) -> 
       let lname, env = add_nom_to_env name envIn in
       let tm, env = t_expr env body in
       P.make_bind lname tm, revert_noms envIn env
       
    | EFun(params, body) ->
       let tm, env = make_lam envIn params body in
       tm, env
                  
    | ENew(name, body) ->
       let lname, env = add_nom_to_env name envIn in
       let tm, env = t_expr env body in
       P.make_new lname tm, revert_noms envIn env

  and t_constant ?(is_pat = false) = function
    | Int(i) -> P.make_int ~is_pat i
    | Bool(b) -> P.make_bool b
    | String(s) -> P.make_string s 
    | EmptyList -> failwith "Should not happen anymore (emptylist)"

  and t_rule envIn = function
    | RSimple(pat, e) -> t_rule envIn (RNa([], pat, e))
    | RNa(names, pat, e) ->
       let pat_noms, env = map_with_env (flip add_nom_to_env) envIn names in
       let pat, env = t_pattern env pat in
       let pattern_vars = env.pattern_vars in
       let env = { env with pattern_vars = [] } in
       let body, env = t_expr env e in
       
       P.make_rule pat_noms pattern_vars pat body,
       revert_noms envIn (revert_patterns envIn (revert_locals envIn env))

  and t_pattern envIn = function
    | PVal(name) -> 
       (* todo: can also be a nominal *)
       begin
         try  
           P.make_pvar name (List.assoc name envIn.pattern_vars), envIn
         with Not_found ->
           let lvar, env = add_to_env name envIn in
           P.make_pvar (fst lvar) (snd lvar),
           { env with pattern_vars = lvar::env.pattern_vars }
       end

    | PAny(_name) -> P.make_app "pany" [], envIn
       
    | PBind(name,pat) ->
       let lname, env = add_nom_to_env name envIn in
       let tm, env = t_pattern env pat in
       P.make_bind ~pattern:true lname tm, revert_noms envIn env
       
    | PApp(_name,_pats) -> failwith "Not implemented: PApp"
                         
    | PBApp(name, pats) ->
       (* todo: maybe the parser should accept more than just names ? *)
       let var, env = t_pattern envIn (PVal(name)) in
       let tms, env = map_with_env t_pattern env pats in
       P.make_appt ~nom:true ~pattern:true var tms, env
                           
    | PConstr(name, pats) ->
       (* A constructor is either 
          a global datatype constructor 
         or a localnominal *)
       begin
         try
           let i = List.assoc name envIn.local_noms in
           if pats = [] then
             P.make_nom ~pattern:true name i, envIn
           else failwith "Hmm, nominal constr do not take arguments"
         with
           Not_found ->
           if List.mem name ctx.global_constrs then
             let tms, env = map_with_env t_pattern envIn pats in
             P.make_constr ~pattern:true name tms, env
           else
             failwith ("Unknown constructor " ^ name)
       end
      
    | PConstant(c) -> t_constant ~is_pat:true c, envIn
    | PListCons(pat1, pat2) ->
       (* todo : not in the interpreter *)
       t_pattern envIn (PConstr("list_cons", [pat1; pat2]))
    | PPair(pat1, pat2) ->
       t_pattern envIn (PConstr("pair", [pat1; pat2]))
  and make_lam env params e =
    (* 
        fun f x y -> body
          => lam (x\ lam (y\ body))
     *)
    let envInitial = env in
    let rec aux env params e =
      match params with
      | [] -> t_expr env e
      | p::ptl -> 
         let lvar, env = add_to_env p env in
         let inner, env = aux env ptl e in
         P.make_lam lvar inner, env
    in
    let tm, env = aux envInitial params e in
    tm, revert_locals envInitial env
  in
  let make_death_list (l : def list) : (string * int) list =
    List.map (fun (d : def) -> (d.name, d.pos.pos_lnum)) l
  in
  t_items p, make_death_list ctx.actual_def





          (*
  let c = ref 0 in
  let constructors = Hashtbl.create 100 in
  let actualDef = { name = "" ;  pos = Lexing.dummy_pos} in
  let allDefs = ref [] in

  let setActualDef n pos =
    actualDef.name <- n;
    actualDef.pos <- pos;
    allDefs := (n, pos.pos_lnum)::!allDefs  
  in
  

  
  let rec aux env = function
      [] -> ("", [], [])
    | item::tl ->
       let name, str, freevars, datatypes =
         (match item with
          | IDef(def, pos) ->
             let name = getDefName def in
             setActualDef name pos;
             let str, freevars, datatypes = aux_def env def in
             name, str, freevars, datatypes
          | IExpr(e, pos)  -> 
             let a = c := !c + 1; !c in
             let name = "val_" ^ (string_of_int a) in
             setActualDef name pos;
             let str, freevars = aux_expr ((Local name)::env) e in
             name, str, freevars, []
         ) in
       let strNext, freevars2, datatypes2 = aux ((Global name)::env) tl in
       (prog_string name str freevars ^ strNext),
       freevars2,
       datatypes @ datatypes2

  and aux_def env = function
    | DLet(lb) ->
       let name, params, code, freevars = aux_lb env lb in
       nofixpoint name params code, freevars, []
    | DLetrec(lb) ->
       let name, params, code, freevars = aux_lb env lb in
       fixpoint name params code, freevars, []
    | DType(name, clist) -> 
       (* When encountering a type declaration, we add the type constructors to 
		the constructors Hashtbl with the arguments arities as a list *)
       List.iter (add_constr constructors name) clist;
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
    | EBApp(e, el) ->
       let str, freevars = (aux_expr env e) in
       let args = List.map (aux_expr env) el in
       
       appv str (List.map (fst) args),
       List.fold_left (fun acc (_, fv) -> acc@fv) freevars args
    | EApp(e, el) ->
       let str, freevars = (aux_expr env e) in
       let args = List.map (aux_expr env) el in
       
       app str (List.map (fst) args),
       List.fold_left (fun acc (_, fv) -> acc@fv) freevars args
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
           then (String.uncapitalize_ascii vp), []
           else vp, []
	 end
    | EPair(e1, e2) ->
       let str1, fv1 = aux_expr env e1
       and str2, fv2 = aux_expr env e2 in
       pair str1 str2, fv1 @ fv2
    | EConstr(n, elist) ->
       let exprs = List.map (aux_expr env) elist in
       let codes = List.map (fst) exprs in
       let fvs = List.flatten (seconds exprs) in
       (try	
	  let _, constr, _ = Hashtbl.find constructors n in
	  
	  (*print_string ("\nFound : " ^n^ "\n");
			List.iter (print_int) constr;*)
	  
	  
	  if (List.fold_left (+) 0 constr) > 0 then
	    (appv n codes), fvs
	  else
	    (appc n codes) , fvs
	with Not_found ->
          raise (makeException ("Unknown constructor \""
                                ^ (strip_prefix n) ^ "\"." 
                               ) actualDef None)
       )
    | EPattern(p) -> let code, pararities = aux_pattern env p in
		     code, firsts pararities
    | EBind(v, e)
      -> add_constr constructors "$_nom" (Simple(v));
         let code, fv = aux_expr (env) e in
         remove_constr constructors (Simple(v));
         bind v code,fv 
    | EFun(v, e) -> let code, fv = aux_expr (Local(v)::env) e in
                    func v code,fv 
    | ENew(v, e) ->
       add_constr constructors "$_nom" (Simple(v));

       (* This ocontructor has arity 0, we can check it's well used : *)
       let aexpr = Arity.maxArityInExpr v e in
       if aexpr > 0 then
         raise (makeException ("Constructor "
                                ^ (strip_prefix v)
                                ^ " has arity 0, not "
                                ^ (string_of_int aexpr)
                                ^ ".") actualDef None);
       
       let code, fv =  aux_expr env e in
       remove_constr constructors (Simple(v));
       newc v code,fv 
                     
  and aux_rule env = function
    | RSimple(p, e) ->
       let pattern, pararity = aux_pattern env p in
       
       let pararity =
         List.map
           (fun (n, a) ->
               n, max (max a  (Arity.maxArityInExpr n e))
                      (Arity.maxArityInPattern constructors n p)
           )
           pararity in
       
       let expr, freevars = aux_expr (metaparams_to_env env pararity) e in
       
       matcher_arrow pararity pattern expr, freevars
                                              
    | RNa(nl, p, e) ->
       List.iter (fun n -> add_constr constructors "$_nom" (Simple(n))) nl;
       let pattern, pararity = aux_pattern env p in
       let pararity =
         List.map (fun (n, a) ->
             n, max (max a (Arity.maxArityInExpr n e))
                    (Arity.maxArityInPattern constructors n p)
           )
                  pararity in

       let expr, freevars = aux_expr (metaparams_to_env env pararity) e in

       List.iter (fun n -> remove_constr constructors (Simple(n))) nl;

       let nl = List.map (String.uncapitalize_ascii) nl in
       
       matcher_na nl pararity pattern expr, freevars
                                              
  and aux_pattern env = function
    | PVal(vn) ->  
       if (List.mem (Nominal(vn)) env) then vn, []
       else if (List.mem (Local(vn)) env) then vn, []
       else if (Hashtbl.mem constructors vn) then vn, []
       else (String.uncapitalize_ascii vn), [(vn, 0)]
    | PBind(vn, p) ->
       add_constr constructors "$_nom" (Simple(vn));
       let code, fv = aux_pattern (env) p in
       remove_constr constructors (Simple(vn));
                      (*print_string (bind vn code); print_pairs (fun s -> s)
                                                               (string_of_int) fv;*)
       bind vn code, fv
    | PBApp(vn, pls) ->
       let pl = List.map (aux_pattern env) pls in
       (*let vn = String.capitalize_ascii vn in*)
       (*print_endline ("Found " ^ vn ^ " ( " ^ (string_of_int (List.length pls))^ " )");*)
       vn ^ (to_separated_list ~first:true " " (firsts pl)) ,
       [vn, List.length pls]
    | PApp(vn, pls) ->
       let pl = List.map (aux_pattern env) pls in
       (*let vn = String.capitalize_ascii vn in*)
       (*print_endline ("Found " ^ vn ^ " ( " ^ (string_of_int (List.length pls))^ " )");*)
       vn ^ (to_separated_list ~first:true " " (firsts pl)) ,
       [vn, List.length pls]
    | PConstr(cp, pls) ->
       (try
          let cp = String.uncapitalize_ascii cp in
	  let tname, aritylist, _ = Hashtbl.find
                             constructors cp
          in

          let sum = List.fold_left (+) 0 aritylist in
          
	  let name = if tname = "$_nom"
                     then cp
                     else specials (cp (*^ "_v"*))
          in
	  let pl = List.map (aux_pattern env) pls in
	  (* TODO CONSTRS IN CONSTRS MAY FAIL *)
	  let params = firsts (List.flatten (seconds pl)) in
          
          let parities = seconds (List.flatten (seconds pl)) in

       (*
                   print_string "\n\nParams: "; print_list (fun x -> x) params;
                   print_string "\nPArities: "; print_list (string_of_int) parities;
                   print_string "\nAritylist: "; print_list (string_of_int) aritylist;
 
           *)
	  (*let marities = List.map2 (fun l1 l2 -> max l1 l2)
                                            parities arities in*)
          let pararity = List.map2 (fun l1 l2 -> l1, l2)
                                   params parities in
          (* let pararity = (List.flatten (seconds pl)) in *)
	  type_constr sum name (firsts pl), pararity
	with Not_found -> 
          raise (makeException ("Unknown constructor \""
                                ^ (strip_prefix cp) ^ "\"." 
                   ) actualDef None)
       )
    | PConstant(c) -> const_to_lpstring c, []
    | PListCons(p1, p2) -> 
       let p1, fv1 = aux_pattern env p1
       and p2, fv2 = aux_pattern env p2 in
       "cns (" ^ p1 ^ ") ("  ^ p2 ^ ")", fv1 @ fv2
    | PPair(p1, p2) -> 
       let p1, fv1 = aux_pattern env p1
       and p2, fv2 = aux_pattern env p2 in
       cpair p1 p2, fv1 @ fv2

  in

  let progs, _, types = aux [] p in
  let evalsig, evalmod, typing
    = Datatypes_translation.translate_types constructors in
  
  
  (* print_string (string_of_constructors constructors);*)
  progs, evalsig, (evalmod ^ typing), !allDefs

           *)

          (*
let make_lp_file prog =
  let progmod = open_out "export/progs_gen.mod" in
  let dtmod = open_out "export/datatypes.mod" in
  let dtsig = open_out "export/datatypes.sig" in
  
  let progs,  types, typingeval, defs = toLPString prog in

  (* progs_gen.mod *)
  output_string progmod "module progs_gen.\n\n";
  output_string progmod progs;

  (* datatypes.mod  *)
  output_string dtmod "module datatypes.\n\n";
  output_string dtmod typingeval; 


  (* datatypes.sig *)
  output_string dtsig "sig datatypes.\naccum_sig eval.
  accum_sig typing.\n\n";
  output_string dtsig types; 

  
  close_out progmod;
  close_out dtmod;
  close_out dtsig*)
    
