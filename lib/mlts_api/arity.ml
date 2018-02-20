open MltsAst

let fold_max_f f =
  List.fold_left
    (fun macc n -> max macc (f n)) 0 
       
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
    | EInfix(e1, _, e2) -> max (aux e1) (aux e2)
    | EConst(_) | EVal(_) -> 0
    | EConstr(vn, el) ->
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
    | PListCons(lp, rp) -> max (aux_pattern lp) (aux_pattern rp)
  in aux
