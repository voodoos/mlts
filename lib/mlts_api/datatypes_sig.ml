open Datatypes

let arity_type_of_atypl =
  let rec aux (ty, i) =
    match ty with
    | Consl(_) -> "tm"
    | Bindl(at1, at2)
      -> let r1, r2 = aux at1, aux at2 in
         "(" ^ r1 ^ " -> " ^ r2 ^ ")"         
    | Listl(at1)
      -> let r1 = aux at1 in
         r1
    | Arrowl(at1, at2)
      -> let r1, r2 = aux at1, aux at2 in
         r1 ^ " -> " ^ r2
    | Suml(l) -> if i > 0 then
                   LpStrings.to_separated_list ~nop:true
                     " -> "
                     (List.map (aux) l)
                 else "tm"
  in
  aux
    
let gen_sig cname atypl =
  let aux_val name =
    "\ntype " ^ name ^ " "
    ^ arity_type_of_atypl atypl
    ^ "."
  in
  let aux_expr (_, i) =
    if i > 0 then aux_val cname
    else "\ntype " ^ cname ^ " tm."
  in
  let tname = match atypl with
    | Arrowl(ty, (Consl(tname), _)), _ -> tname
    | _, _ -> lp_typ_of_atypl atypl
  in
  
  "\ntype " ^ tname ^ " ty."
  ^ (aux_expr atypl)
  ^ (aux_val (cname ^ "_v"))     
