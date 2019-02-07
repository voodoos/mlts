open MltsAst

module P = PrologAst

exception TranslatorError of string * (Lexing.position option)
exception StaticCheckError of string * (Lexing.position option)

type def = { name: string; pos: Lexing.position }

let strip name = String.sub name 2 (String.length name - 2)

let make_exception message def pos =
  TranslatorError("In \"" ^ (strip def.name) ^ "\" (line "
                  ^ (string_of_int def.pos.pos_lnum)
                  ^ ") : " ^ message,
                  match pos with
                    None -> Some(def.pos)
                  | Some(_p) -> pos)



let make_sc_exception message def pos =
  StaticCheckError("In \"" ^ (strip def.name) ^ "\" (line "
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
    mutable wanabeeRigidNoms: P.local_name list;
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
              actual_def = [];
              wanabeeRigidNoms = []
            }
  in
  let add_global v = ctx.global_vars <- v::ctx.global_vars in
  let add_constr c = ctx.global_constrs <- c::ctx.global_constrs in
  let set_actual_def ctx name pos =
    ctx.actual_def <- { name; pos } :: ctx.actual_def
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
       t_def env pos def
    | IExpr(expr, pos) ->
       let name = "val_" ^ (string_of_int (ctx.nb_expr)) in
       set_actual_def ctx name pos;
       incr_expr ctx;
       let texpr, env = t_expr env expr in
       [P.Definition({
         name = "prog";
         args = [P.Lit(P.String(name));
                 texpr];
         (* Some programs may depend on others! *)
         body = P.make_deps (env.free_vars)
       })]

  and t_def env pos =
    function
    | DLet(LBVal(name, params, e)) ->
       set_actual_def ctx name pos;
       let body, env = make_lam env params e in
       add_global name;
       [P.Definition({
             name = "prog";
             args = [P.Lit(P.String(name)); body];
             body = P.make_deps (env.free_vars);
       })]

    | DLetrec([LBVal(name, params, e)]) ->
       set_actual_def ctx name pos;
       let ln, env = add_to_env name env in
       let body, env = make_lam env params e in
       add_global name;
       [P.Definition({
             name = "prog";
             args = [P.Lit(P.String(name));
                     P.make_fix ln body];
             body = P.make_deps (env.free_vars);
       })]

    | DLetrec(mutuals) ->
        set_actual_def ctx "mutual function" pos;
        let name = List.fold_left (
          fun acc (LBVal(name, _, _)) -> acc ^ name
          ) "m_" mutuals in
        failwith "Mutual rec not implemented"

    | DType(name, decls) ->
      set_actual_def ctx name pos;
      let rec list_of_sum ty =
        let rec t_typ = function
          | Cons(c) -> P.make_global c
          | Pair(ty1, ty2) -> P.make_app "t_pair" [t_typ ty1; t_typ ty2]
          | Sum(_, _) as s -> P.List (List.rev (list_of_sum s))
          | Arrow(ty1, ty2) -> P.make_app "arr" [t_typ ty1; t_typ ty2]
          | Bind(ty1, ty2) -> P.make_app "bigarr" [t_typ ty1; t_typ ty2]
          | List(ty) -> P.make_app "t_list" [t_typ ty](*failwith "List (type) not implemented"*)
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
       let args, env = map_with_env t_nom env args in
       P.make_nom_appt te args, env

    | EInfix(e1, op, e2) ->
       let te1, env = t_expr envIn e1 in
       let te2, env = t_expr env e2 in
       P.make_spec op [te1; te2],
       env

    | EConst(c) -> t_constant c, envIn

    | EVal(v) ->
       begin
         (* print_env envIn; *)
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
       ctx.wanabeeRigidNoms <- pat_noms;
       let pat, env = t_pattern env pat in

       if ctx.wanabeeRigidNoms != [] then
         raise (make_sc_exception
                  (String.concat ""
                     ["Nabla-bound nominal(s) ";
                      (String.concat ", "
                         (List.map (fun e -> e |> fst |> strip) ctx.wanabeeRigidNoms));
                     " must have a rigid occurence in the pattern."])
               (List.hd ctx.actual_def)
               None);
       let pattern_vars = env.pattern_vars in
       let env = { env with pattern_vars = [] } in
       let body, env = t_expr env e in

       P.make_rule pat_noms pattern_vars pat body,
       revert_noms envIn (revert_patterns envIn (revert_locals envIn env))

  and t_nom env nom =
    P.make_nom nom (List.assoc nom env.local_noms), env

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

    | PBApp(name, args) ->
       (* todo: maybe the parser should accept more than just names ? *)
       let var, env = t_pattern envIn (PVal(name)) in
       let noms, env = map_with_env t_nom env args in
       P.make_appt ~nom:true ~pattern:true var noms, env

    | PConstr(name, pats) ->
       (* A constructor is either
          a global datatype constructor
         or a localnominal *)
       begin
         try
           let i = List.assoc name envIn.local_noms in
           if pats = [] then
             (ctx.wanabeeRigidNoms <-
               List.remove_assoc name ctx.wanabeeRigidNoms;
             P.make_nom ~pattern:true name i, envIn)
           else failwith "Hmm, nominal constr do not take arguments"
         with
           Not_found ->
           if List.mem name ctx.global_constrs then
             let tms, env = map_with_env t_pattern envIn  pats in
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

  let items = t_items p in
  let defs = make_death_list ctx.actual_def in

  items, defs
