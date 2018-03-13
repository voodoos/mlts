open Datatypes

let make_pi ars ats =
  let make_pis =
    List.map2
      (fun a t -> "pi " ^ a ^ "\\ "
                  ^ "typeof " ^ a
                  ^ " " ^ (lp_typ_of_atypl t))
  in
  LpStrings.to_separated_list
    ~nop:true
    " => "
    (make_pis ars ats)
    
let make_typeof_of_list tname =
  let aux i (ty, a) =
    if a > 0 then
      let args = make_args_from_int ~sym:"x" a in
      let argstyps = get_args_typs (ty, a) in
      let pis = make_pi args argstyps in
      "(" ^ pis ^ " => typeof (X"
      ^ (string_of_int i)
      ^ " "
      ^ (LpStrings.to_separated_list ~nop:true " " args)
      ^ ") " ^ tname ^ ")"
    else "typeof X" ^ (string_of_int i)
         ^ " (" ^ (lp_typ_of_atypl (ty, a)) ^ ")"
  in List.mapi (aux)

let typeof_val arity cname tname args typofs =
  let open LpStrings in
  "\ntypeof (" ^ cname  ^ " "
  ^ (if (arity > 0) then (to_separated_list ~nop:true " " args)
    else LpStrings.to_pr args)
  ^ ") " ^ tname
  ^ (if List.length args > 0 then
       " :- "
       ^ (to_separated_list ", " typofs)
       ^ "."
     else ".")
      
let gen_typeof_preds cname atypl =
  
  let rec aux_expr (typ, arity) = 
    if arity > 0 then
      aux_val cname (typ, arity)
    else
      "\ntypeof "
      ^ cname ^ " "
      ^ (lp_typ_of_atypl (typ, arity))
      ^ "."
          
  and aux_val cname (typ, i) =
    let thety, tname, bla = match typ with
      | Arrowl(ty, (Consl(tname), _)) -> ty, tname, base_level_arities ty
      | _ -> (typ, i), lp_typ_of_atypl (typ, i), []
    in
    let arg_list = make_args_from_list bla in
    let right_typeof_list = make_typeof_of_list tname bla in
    (*"\nbla:" ^ (print_bal bla )^*)
    (typeof_val i cname tname arg_list right_typeof_list)
  in
  (aux_expr atypl)
  ^ (aux_val (cname ^ "_v") atypl) ^ "\n"
