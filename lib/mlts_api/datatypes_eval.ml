open Datatypes

let rec dashes = function
    0 -> ""
  | 1 -> "_"
  | n -> "_ " ^ (dashes (n - 1))
                  

let make_copy_clause cname argsx argsy clauses =
  "\ncopy ("^ cname
  ^ (LpStrings.to_separated_list ~first:true ~nop:true " " argsx)
  ^ ") ("^ cname
  ^ (LpStrings.to_separated_list ~first:true ~nop:true " " argsy)
  ^ ")"
  ^ (if (List.length clauses > 0) then
      " :- "
      ^ (LpStrings.to_separated_list ~nop:true ", " clauses)
    else "")
  ^ "."

let make_pi ars =
  let make_pis =
    List.map
      (fun a -> "pi " ^ a ^ "\\ "
                  ^ "copy " ^ a
                  ^ " " ^ a)
  in
  LpStrings.to_separated_list
    ~nop:true
    " => "
    (make_pis ars)

let make_copy_of_list =
  let aux i (ty, a) =
    let si = string_of_int i in
    if a > 0 then
      let args = make_args_from_int ~sym:"x" a in
      let pis = make_pi args in
      let arglist =  LpStrings.to_separated_list ~nop:true " " args in
      "(" ^pis ^ " => copy  (X"
      ^ (string_of_int i)
      ^ " "
      ^ arglist
      ^ ") (Y"
      ^ (string_of_int i)
      ^ " "
      ^ arglist
      ^ "))"
    else
      "copy X" ^ si ^ " Y" ^ si
  in
  List.mapi (aux)

let make_pin ars =
  let make_pins =
    List.map
      (fun a -> "pin " ^ a ^ "\\ ")
  in
  LpStrings.to_separated_list
    ~nop:true
    " "
    (make_pins ars)

let fixbugs args =
  String.concat "" (List.map (fun s -> "fixbug " ^ s ^ ", ") args)
            
let make_eval_of_list =
  let aux i (ty, a) =
    let si = string_of_int i in
    if a > 0 then
      let args = make_args_from_int ~sym:"x" a in
      let pins = make_pin args in
      let arglist =  LpStrings.to_separated_list ~nop:true " " args in
      "(" ^ pins ^ " eval  (X"
      ^ (string_of_int i)
      ^ " "
      ^ arglist
      ^ ") (Y"
      ^ (string_of_int i)
      ^ " "
      ^ arglist
      ^ "))"
    else
      "eval X" ^ si ^ " Y" ^ si
  in
  List.mapi (aux)

let make_eval_clause cname argsx argsy evals =
  "\neval ("^ cname
  ^ (LpStrings.to_separated_list ~first:true ~nop:true " " argsx)
  ^ ") ("^ cname ^ "v"
  ^ (LpStrings.to_separated_list ~first:true ~nop:true " " argsy)
  ^ ")"
  ^ (if (List.length evals > 0) then
       " :- "
       ^ (fixbugs argsx)
       ^ (LpStrings.to_separated_list ~nop:true ", " evals)
     else "bug")
  ^ "."
      
let make_special cname n =
  let sn = string_of_int n in
  let args = make_args_from_int n in
  let largs = List.rev args in
  if n > 0 then
    "\nspecial " ^ sn ^ " " ^ cname ^ "."
    ^ "\neval_spec " ^ cname ^ " ("
    ^ (LpStrings.to_separated_list ~nop:true "::" largs)
    ^ "::[]) ("
    ^ cname ^ "v " ^ (LpStrings.to_separated_list ~nop:true " " args) ^ ")."
  else "\neval " ^ cname ^ " " ^ cname ^ "v."



                                           
let gen_eval_preds cname atypl =
  let thety, tname, bla = match atypl with
    | Arrowl(ty, (Consl(tname), _)), _ -> ty, tname,  base_level_arities ty
    | _, _ -> atypl, lp_typ_of_atypl atypl, []
  in
  let ty, i = thety in
  
  let gen_val () =
    let n = List.length bla in
    "\nval (" ^ cname ^ "v"
    ^ (if n > 0 then (" " ^ (dashes n)) else "")
    ^ ")."
  in
  
  let gen_copy () =
    let copy cname =
      let args_x = make_args_from_list bla in
      let args_y = make_args_from_list ~sym:"Y" bla in
      let clauses = make_copy_of_list bla in
      (make_copy_clause cname args_x args_y clauses)
    in
    (copy (cname ^ "v"))
    ^ (copy cname)
  in
  let gen_eval () =
    if i > 0 then
      let args_x = make_args_from_list bla in
      let args_y = make_args_from_list ~sym:"Y" bla in
      let evals = make_eval_of_list bla in
      (make_eval_clause cname args_x args_y evals)
    else
      make_special cname (List.length bla)
  in
  
  (gen_val ())
  ^  (gen_copy ())
  ^  (gen_eval ())
