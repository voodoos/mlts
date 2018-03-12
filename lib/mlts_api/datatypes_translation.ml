open MltsAst


let translate_type name (tn, arities, typ)
                   (acc_typing, acc_eval, acc_sig) =
  let cs = Datatypes.atypl_of_aritytypexpr tn typ in
  let ntyping = Datatypes_typeof.gen_typeof_preds name cs and
      neval = Datatypes_eval.gen_eval_preds name cs and
      nsig = Datatypes_sig.gen_sig name cs
  in
  ntyping ^ acc_typing, neval ^ acc_eval, nsig ^ acc_sig
                                                   
let translate_types constr =
  (*
  let typing =  Hashtbl.fold types_typing constr "" in
  let evalmod = Hashtbl.fold types_mod constr "" in
  let evalsig = Hashtbl.fold types_sig constr "" in
   *)
  let typing, evalmod, evalsig =  Hashtbl.fold translate_type constr ("", "", "") in
  evalsig, evalmod, typing
