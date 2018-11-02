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
type context = { mutable nb_expr: int; mutable global_vars : string list }
let incr_expr c = c.nb_expr <- c.nb_expr + 1

(* (local) Environments *)
type env = { local_vars: P.local_name list; free_vars:  P.global_name list }
let add_to_env v env = 
  match List.assoc_opt v env.local_vars with
  | None -> (v, 0), { env with local_vars = (v, 0)::env.local_vars }
  | Some(i) -> (v, i + 1), { env with local_vars = (v, i + 1)::env.local_vars }
let first_in_env v env = List.assoc v env.local_vars

type tdef = { name: string; body: P.term; env: env }

(* PROGRAM TRANSLATION *)
let mlts_to_prolog p =
  let ctx = { nb_expr = 0; global_vars = [] } in
  let add_global v = ctx.global_vars <- v::ctx.global_vars in

  let rec t_items = function
    | [] -> []
    | item::tl -> 
       let titem = t_item item in
       P.Definition(titem)::(t_items tl)

  and t_item =
    (* Each "toplevel" item transpile to a program *)
    (* Each "toplevel" item ha its own env but shares
         the global context *)
    let env = { local_vars = []; free_vars = [] } in
    function
    | IDef(def, _pos) -> 
       let tdef = t_def env def in
       add_global tdef.name;
       {
         name = "prog";
         args = [P.Lit(P.String(tdef.name));
                 P.make_arity 0; (* todo *)
                 tdef.body];
         (* Some programs may depend on others! *)
         body = P.make_deps (tdef.env.free_vars)
       }
    | IExpr(expr, _pos) -> 
       incr_expr ctx; 
       let texpr, env = t_expr env expr in
       {
         name = "prog";
         args = [P.Lit(P.String(
                           "val_"
                           ^ (string_of_int (ctx.nb_expr)))); 
                 P.make_arity 0; (* todo *)
                 texpr];
         (* Some programs may depend on others! *)
         body = P.make_deps (env.free_vars)
       }

  and t_def env =
    let rec make_lam env params e =
      (* 
        fun f x y -> body
          => lam ar11 ar12 (x\ lam ar21 ar22 (y\ body))
       *)
      match params with
      | [] -> t_expr env e
      | p::ptl -> 
         let lvar, env = add_to_env p env in
         let inner, env = make_lam env ptl e in
         P.make_lam 0 0 lvar inner, env 
    in
    function
    | DLet(LBVal(name, params, e)) -> 
       let body, env = make_lam env params e in
       { 
         name; 
         body;
         env;
       }
    | DLetrec(_) -> failwith "Not implemented: DLetrec"
    | DType(_, _) -> failwith "Not implemented: DType"
                   
  and t_expr env = function
    | ELetin(LBVal(_name, _params, _e), _body) -> failwith "Not implemented: ELetin"
    | ELetRecin(_, _) -> failwith "Not implemented: ELetRecin"
    | EMatch(_, _) -> failwith "Not implemented: EMatch"
    | EIf(_, _, _) -> failwith "Not implemented: EIf"
    | EApp(e, args) -> 
       let te, env = t_expr env e in
       let args = List.map (t_expr env) args in
       (* todo freevars can appear in arg, don't forget it... *)
       let args = List.map (fst) args in
       P.make_app te args, env
    | EBApp(_, _) -> failwith "Not implemented: EBApp"
    | EInfix(e1, op, e2) -> 
       let te1, env = t_expr env e1 in
       let te2, env = t_expr env e2 in
       P.make_spec (LpStrings.infix_to_lpstring op) [te1; te2], env
    | EConst(c) -> t_constant c, env
    | EVal(v) -> 
       begin
         try P.make_local v (first_in_env v env), env
         with Not_found -> (* Non-local must be global *)
           if (List.mem v ctx.global_vars) then
             P.make_global v, { env with free_vars = v::env.free_vars }
           else failwith ("Non local, non global value \""
                          ^ v
                          ^ "\". (todo : nice exception)")
       end
    | EPair(_, _) -> failwith "Not implemented: EPair"
    | EConstr(_, _) -> failwith "Not implemented: EConstr"
    | EPattern(_) -> failwith "Not implemented: EPattern"
    | EBind(_, _) -> failwith "Not implemented: EBind"
    | EFun(_, _) -> failwith "Not implemented: EFun"
    | ENew(_, _) -> failwith "Not implemented: ENew"

  and t_constant = function
    | Int(i) -> P.make_int i
    | Bool(b) -> P.make_bool b
    | String(s) -> P.make_string s 
    | EmptyList -> failwith "Not implemented: EmptyList"
  in
  t_items p





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
    
