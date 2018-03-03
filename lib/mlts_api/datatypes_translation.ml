let rec tm_list = function
    0 -> ""
  | i -> "tm -> " ^ (tm_list (i - 1))

let rec dash_list = function
    0 -> ""
  | i -> "_ " ^ (dash_list (i - 1))

                  
let rec var_list n = function
    0 -> ""
  | i -> n ^ (string_of_int i) ^ " " ^ (var_list n (i - 1))
                                         
let rec fixbug_list n = function
    0 -> ""
  | i -> "fixbug " ^ n ^ (string_of_int i) ^ ", " ^ (var_list n (i - 1))
                                         
let rec var_listc n = function
    0 -> ""
  | i -> n ^ (string_of_int i) ^ "::" ^ (var_listc n (i - 1))

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


                              

let types_sig name arities acc =
  "\ntype " ^ name ^ " "
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
         acc ^ ("("
                ^ (tm_list i)
                ^ "tm) -> ")
       ) "" arities)
  ^ "tm." ^ acc


let rec copy_list n m = function
    [] -> ""
  | i::tl ->
     (if i = 0 then
        begin
          "copy " ^ n ^ (string_of_int i)
          ^ " " ^ m ^ (string_of_int i)
        end
      else
        begin
          (pi_list i)
          ^ "copy (" ^ n ^ (string_of_int i)
          ^ " " ^ (var_list "x" i)
          ^ ") (" ^ m ^ (string_of_int i)
          ^ " " ^ (var_list "x" i)
          ^ ") "
        end
     )
     ^ (if List.length tl >= 1 then ", " else "")
     ^ (copy_list n m tl)

       
              
let copy name ars n =
  "\ncopy (" ^ name ^ " " ^ (var_list "X" n) ^ ")"
  ^ " (" ^ name ^ " " ^ (var_list "Y" n) ^ ")"
  ^ " :- " ^ (copy_list "X" "Y" ars) ^ "."
              
let copy_clauses name arities n =
  (if (List.fold_left (+) 0 arities) > 0 then
      (copy (name) arities n)
   else "")
  ^ (copy (name ^ "v") arities n)

      
let rec eval_list n m = function
    [] -> ""
  | i::tl ->
     (if i = 0 then
        begin
          "eval " ^ n ^ (string_of_int i)
          ^ " " ^ m ^ (string_of_int i) 
        end
      else
        begin
          (pin_list i)
          ^ "eval (" ^ n ^ (string_of_int i)
          ^ " " ^ (var_list "x" i)
          ^ ") (" ^ m ^ (string_of_int i)
          ^ " " ^ (var_list "x" i)
          ^ ") "
        end
     )
     ^ (if List.length tl >= 1 then ", " else "")
     ^ (eval_list n m tl)
         
let specialp name arities n =
  "\nspecial " ^ (string_of_int n) ^ " " ^ name ^ "."
  ^ "\neval_spec " ^ name ^ " (" ^ (var_listc "X" n) ^ "nil) ("
  ^ name ^ "v " ^ (var_list "X" n) ^ ")."

let eval_apply name arities n =
  "\neval (" ^ name ^ " " ^ (var_list "X" n) ^ ") ("
  ^ name ^ "v " ^ (var_list "Y" n) ^ ") :- "
  ^ fixbug_list "X" n ^ eval_list "X" "Y" arities
  ^ "." 

let eval_clauses name arities n =
  if (List.fold_left (+) 0 arities) > 0 then
    eval_apply name arities n
  else 
    specialp name arities n
              
let types_mod name arities acc =
  let na = List.length arities in
  let copy = copy_clauses name arities na in
  let eval = eval_clauses name arities na in
  "\nval (" ^ name ^ "v " ^ (dash_list na) ^ ")."
  ^ copy
  ^ eval
  ^ acc


let translate_types constr =
  let typing = "" in
  let evalmod = Hashtbl.fold types_mod constr "" in
  let evalsig = Hashtbl.fold types_sig constr "" in
  evalsig, evalmod, typing
