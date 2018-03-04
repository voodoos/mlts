open MltsAst
let rec tm_list = function
    0 -> ""
  | i -> "tm -> " ^ (tm_list (i - 1))

let rec dash_list = function
    0 -> ""
  | i -> "_ " ^ (dash_list (i - 1))

                  
let rec var_list na i =
  let rec aux = function
    0 -> ""
    | n -> na ^ (string_of_int (i - n + 1)) ^ " " ^ (aux (n - 1))
  in aux i
                                         
let rec fixbug_list na i = 
  let rec aux = function
      0 -> ""
    | n -> "fixbug " ^ na ^ (string_of_int n) ^ ", " ^ (aux (n - 1))
  in aux i
                                         
let rec var_listc na i =
  let rec aux = function
    0 -> ""
    | n -> na ^ (string_of_int n) ^ "::" ^ (aux (n - 1))
  in aux i

let rec pi_list = function
    0 -> ""
  | i -> "pi x" ^ (string_of_int i)
         ^ " \\ copy x" ^ (string_of_int i)
         ^ " x" ^ (string_of_int i)
         ^" => "^ (pi_list (i - 1))

                    
let rec pin_list = function
    0 -> ""
  | i -> "pin x" ^ (string_of_int i)
         ^ " \\ "^ (pin_list (i - 1))


                              

let types_sig name (tn, arities, _) acc =
  let name = String.uncapitalize_ascii name in
  "\ntype " ^tn ^ " ty."
  ^ "\ntype " ^ name ^ " "
  ^ (List.fold_left (fun acc i ->
         if i = 0 then acc
         else (
           acc ^ ("("
                  ^ (tm_list i)
                  ^ "tm) -> ")
       )) "" arities)
  ^ "tm."
  ^ "\ntype " ^ name ^ "v "
  ^ (List.fold_left (fun acc i ->
         if i = 0 then 
           acc ^ ("tm -> ")
         else (
           acc ^ ("("
                  ^ (tm_list i)
                  ^ "tm) -> ")
         )
       ) "" arities)
  ^ "tm." ^ acc


let rec copy_list c n m = function
    [] -> ""
  | i::tl ->
     (if i = 0 then
        begin
          "copy " ^ n ^ (string_of_int c)
          ^ " " ^ m ^ (string_of_int c)
        end
      else
        begin
          (pi_list i)
          ^ "copy (" ^ n ^ (string_of_int c)
          ^ " " ^ (var_list "x" i)
          ^ ") (" ^ m ^ (string_of_int c)
          ^ " " ^ (var_list "x" i)
          ^ ") "
        end
     )
     ^ (if List.length tl >= 1 then ", " else "")
     ^ (copy_list (c + 1) n m tl)

       
              
let copy name ars n =
  "\ncopy (" ^ name ^ " " ^ (var_list "X" n) ^ ")"
  ^ " (" ^ name ^ " " ^ (var_list "Y" n) ^ ")"
  ^ " :- " ^ (copy_list 1 "X" "Y" ars) ^ "."
              
let copy_clauses name arities n =
  (if (List.fold_left (+) 0 arities) > 0 then
      (copy (name) arities n)
   else "")
  ^ (copy (name ^ "v") arities n)

      
let rec eval_list c n m = function
    [] -> ""
  | i::tl ->
     (if i = 0 then
        begin
          "eval " ^ n ^ (string_of_int c)
          ^ " " ^ m ^ (string_of_int c) 
        end
      else
        begin
          (pin_list i)
          ^ "eval (" ^ n ^ (string_of_int c)
          ^ " " ^ (var_list "x" i)
          ^ ") (" ^ m ^ (string_of_int c)
          ^ " " ^ (var_list "x" i)
          ^ ") "
        end
     )
     ^ (if List.length tl >= 1 then ", " else "")
     ^ (eval_list (c + 1) n m tl)
         
let specialp name arities n =
  "\nspecial " ^ (string_of_int n) ^ " " ^ name ^ "."
  ^ "\neval_spec " ^ name ^ " (" ^ (var_listc "X" n) ^ "[]) ("
  ^ name ^ "v " ^ (var_list "X" n) ^ ")."

let eval_apply name arities n =
  "\neval (" ^ name ^ " " ^ (var_list "X" n) ^ ") ("
  ^ name ^ "v " ^ (var_list "Y" n) ^ ") :- "
  ^ fixbug_list "X" n ^ eval_list 1 "X" "Y" arities
  ^ "." 

let eval_clauses name arities n =
  if (List.fold_left (+) 0 arities) > 0 then
    eval_apply name arities n
  else 
    specialp name arities n
              
let types_mod name (tn, arities, _) acc =
  let na = List.length arities in
  let name = String.uncapitalize_ascii name in
  let copy = copy_clauses name arities na in
  let eval = eval_clauses name arities na in
  "\nval (" ^ name ^ "v " ^ (dash_list na) ^ ")."
  ^ copy
  ^ eval
  ^ acc


(* TODO : Work only for typ => typ not for example string => typ *)
let rec pitype_list tn = function
    0 -> ""
  | i -> "pi x" ^ (string_of_int i)
         ^ " \\ typeof x" ^ (string_of_int i)
         ^ " " ^ tn
         ^" => "^ (pi_list (i - 1))
                    
let rec typing_list c n tn = function
    [] -> ""
  | i::tl ->
     (if i = 0 then
        begin
          "typeof " ^ n ^ (string_of_int c)
          ^ " " ^ tn
        end
      else
        begin
          (pitype_list tn i)
          ^ "typeof (" ^ n ^ (string_of_int c)
          ^ " " ^ (var_list "x" i)
          ^ ") " ^ tn 
        end
     )
     ^ (if List.length tl >= 1 then ", " else "")
     ^ (typing_list (c + 1) n tn tl)
      
let typing_val name tn arities n =
  "\ntypeof (" ^ name ^ " " ^  (var_list "X" n) ^ ") "
  ^ tn ^ " :- " ^ (typing_list 1 "X" tn arities) ^ "."

let rec arrows tn = function
  |  1 -> "arr " ^ tn ^ " " ^ tn 
  | n -> "arr " ^ tn ^ " (" ^ (arrows tn (n - 1)) ^ ")"
   
let typing_exp name tn arities n =
  if (List.fold_left (+) 0 arities) > 0 then
    typing_val name tn arities n
  else 
    "\ntypeof " ^ name ^ " ("
    ^ (arrows tn (List.length arities)) ^ ")."


let typeof_1 name tn arities n (typ, a) =
  let rec typing_list2 = function
      Cons(c) -> "typeof " ^ c ^ " " ^ tn ^ " "
    | Sum((t1, a1), (t2, a2)) -> (typing_list2 t1) ^ ", " ^ (typing_list2 t2)
    | Bind((t1, a1), (t2, a2)) ->
       "pi x\\ (" ^ (typing_list2 t1)
                                  ^ " => " ^ (typing_list2 t2) ^ ")"
  in
  "\ntypeof (" ^ name ^ " " ^ (var_list "X" n) ^ ") " ^ tn ^ " :- "
  ^ (typing_list2 typ)
    

                                            
  
let types_typing name (tn, arities, typ) acc =
  let n = List.length arities in
  let name = String.uncapitalize_ascii name in
  (typing_val (name ^ "v") tn arities n)
  ^ (typing_exp name tn arities n)
  (*   ^ "\n\nPouet : " ^ (typeof_1 name tn arities n typ) ^"\n\n" *)
  ^ acc

let translate_types constr =
  let typing = Hashtbl.fold types_typing constr "" in
  let evalmod = Hashtbl.fold types_mod constr "" in
  let evalsig = Hashtbl.fold types_sig constr "" in
  evalsig, evalmod, typing
