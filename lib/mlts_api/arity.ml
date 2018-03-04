open MltsAst

let fold_max_f f =
  List.fold_left
    (fun macc n -> max macc (f n)) (-1) 
       
let maxArityInExpr name =
  let rec aux = function
    | ELetin(lb, e) | ELetRecin(lb, e) ->
       max (aux_lb lb) (aux e)
    | EMatch(e, pm) -> max (aux e)
                           (fold_max_f (aux_rule) pm)
    | EIf(e1, e2, e3) ->
       max (aux e1) (max (aux e2) (aux e3))
    | EApp(e, argl) ->
       let maxa = fold_max_f (aux) argl in
       (match e with
       | EVal(n) when n = name ->
          max (List.length argl) maxa
       | _ -> maxa)
    | EInfix(e1, _, e2) | EPair(e1, e2) -> max (aux e1) (aux e2)
    | EConst(_) | EVal(_) -> 0
    | EConstr(vn, el) ->
       (* TODO seems wrong, see maxArityInPattern *)
       let maxe = fold_max_f (aux) el in
       if vn = name then
         max (List.length el) maxe
       else maxe
    | EPattern(p) -> aux_pattern p
    | EBind(_, e) | EFun(_, e) | ENew(_, e) ->
       aux e
                             
  and aux_lb = function
    | LBVal(_, _, e) -> aux e
  and aux_rule = function
    | RSimple(p, e) | RNa(_, p, e) ->
       max (aux_pattern p) (aux e)
  and aux_pattern = function
    | PVal(_) | PConstant(_) -> 0
    | PApp(vn, pl) | PConstr(vn, pl) ->
       let maxp = fold_max_f (aux_pattern) pl in
       if vn = name then
         max (List.length pl) maxp
       else maxp
    | PBind(_, rp) -> aux_pattern rp
    | PListCons(lp, rp) | PPair(lp, rp) -> max (aux_pattern lp) (aux_pattern rp)
                               
  in aux

let maxArityInPattern constrs name =
  let rec aux = function
    | PVal(n) when n = name -> 0
    | PVal(_) | PConstant(_) -> -1
    | PBind(n, p) -> aux p
    | PApp(n, pl) when n = name
      -> List.length pl
    | PApp(_, pl) -> fold_max_f aux pl
    | PConstr(c, pl) ->
       let _, expected_arities, _ =
         Hashtbl.find constrs
                      (String.uncapitalize_ascii c) in
       let pl_arities = List.map2 (maxArityUnderConstr)
                                  expected_arities
                                  pl in
       List.fold_left (max) 0 pl_arities
    | PListCons(p1, p2) | PPair(p1, p2)
      -> max (aux p1) (aux p2)
             
  and maxArityUnderConstr exp pattern =
    match pattern with
    | PVal(n) when n = name
      -> exp
    | PVal(_) | PConstant(_) -> -1
    | PBind(_, p) -> aux p
    | PApp(n, pl) when n = name
      -> exp - (List.length pl)
    | PApp(_, pl) -> fold_max_f (aux) pl
    | PConstr(c, pl)
      -> 
       let _, expected_arities, _ =
         Hashtbl.find constrs
                      (String.uncapitalize_ascii c)in
       let pl_arities = List.map2 (maxArityUnderConstr)
                                  expected_arities
                                  pl in
       List.fold_left (max) 0 pl_arities
    | PListCons(p1, p2) | PPair(p1, p2)
      -> max (aux p1) (aux p2)
             
  in aux
