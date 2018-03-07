open MltsAst

let strip_prefix s = String.sub s 2 (String.length s - 2)
       
(* Some tools for list management *)

let print_pairs fts sts =
  List.iter (fun (a,b) -> print_string ("(" ^ (fts a) ^ ", " ^ (sts b) ^ ");"))

            
let print_list fts =
  List.iter (fun a -> print_string ((fts a) ^ ";"))

let to_separated_list ?first:(f = false) ?nop:(nop = false)  s l =
  let rec aux = function
    | [] -> ""
    | [x] -> if nop then x else "(" ^ x ^ ")"
    | x::tl -> if nop then x ^ s ^ (aux tl)
               else "(" ^ x ^ ")" ^ s ^ (aux tl)
  in
  if f && l != [] then s ^ (aux l) else (aux l)
  
let firsts l = List.map (fst) l
let seconds l = List.map (snd) l

(* Utilities to construct strings of LP *)

let specials = 
  let h = Hashtbl.create 17 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
            [ "List.v_hd", "car";
              "List.v_tl", "cdr";
              (*"App", "app";
              "Appv", "ap";
              "Abt", "abt";
              "Abtv", "ab"*)] ;
  fun s ->
  try  Hashtbl.find h s
  with Not_found -> s
                             
let const_to_lpstring = function
  | Int(i) -> "(i " ^ (string_of_int i) ^ ")"
  | Bool(true) -> "tt"
  | Bool(false) -> "ff"
  | EmptyList ->  "null"
                              
let infix_to_lpstring = function
  | Mult -> "times" | Neq -> "nequal"
  | Lt -> "lt" | Le -> "le"
  | And -> "and" | Or -> "or"
  | Add -> "sum" | Minus -> "minus"
  | Equal -> "equal"
  | ListCons -> "cons"

let prog_string name code freevars =
  if code <> "" then (
  "prog \"" ^ name ^ "\""
  ^ " (" ^ code ^ ")"
  ^ ( let sizeEnv = List.length freevars in
      if sizeEnv > 0 then begin
          let count = ref sizeEnv in
          " :- "
          ^ List.fold_left (fun acc s ->
              count := !count - 1;
              acc ^ "prog \"" ^ s ^ "\" " ^ (*s ^ ", "
              ^ "eval P" ^ s ^ " " ^*) String.capitalize_ascii s
              ^ (if !count > 0 then ", " else ""))
                         ""
                         freevars
        end
      else "")
  ^ "." 
  (*^ " % " ^ ( to_separated_list ", " freevars)*)
  ^ "\n\n" ) else ""

let let_binding_val name args code =
  name ^ "\\"
  ^  (List.fold_left (fun acc p -> acc ^ "lam " ^ p ^ "\\ ") " " args)
  ^ code
      
      
let lams  args =
  (List.fold_left (fun acc p -> acc ^ "lam " ^ p ^ "\\ ") " " args)
      
let funcl name args =
  name ^ "\\"
  ^  (lams args)
       
let nofixpoint name args code =
  "(" ^ lams args ^ code ^ ")"
                                        
let fixpoint name args code =
  "(fixpt " ^ funcl name args ^ code ^ ")"

let letin name args code body =
  "(let (" ^ code ^ ") (" ^ funcl name args ^ " " ^ body ^ "))"

                                                            
let letrecin name args code body =
  "(let (fixpt " ^ funcl name args ^ " (" ^ code ^ ")) (" ^ name ^ "\\ (" ^ body ^ ")))"
      

let if_then_else cond e1 e2 =
  "cond (" ^ cond ^ ") "
  ^ "\n (" ^ e1 ^ ") "
  ^ "\n (" ^ e2 ^ ") " 

let binop op e1 e2 =
  (infix_to_lpstring op) ^ " arobase (" ^ e1 ^ ") arobase (" ^ e2 ^ ")"

let list_of_rules rules =
  "\n["
  ^ List.fold_left (fun acc r -> acc ^ ", \n" ^ r) (List.hd rules) (List.tl rules)
  ^ "]"


let matcher_arrow ?nabs:(lnab = []) pararities pattern expr =
  "("
  ^ (List.fold_left (fun acc (p, a) -> 
		acc 
		^ (match a with
			0 -> "all "
			| 1 -> "all' "
			| 2 -> "all'' "
			| _ -> "ALL? " )
		^ (p) ^ "\\ ") "" pararities)
  ^ (if (List.length lnab >= 0) then
      (List.fold_left (fun acc x -> acc ^ "nab " ^ x ^ "\\ ") "" lnab)
    else "") ^ 

  " ((" ^ pattern ^ ") ==> (" ^ expr ^ ")))"
  
let matcher_na namel pararities pattern expr =
  matcher_arrow ~nabs:namel pararities pattern expr

let bind v  e =
  "(" ^ v ^ "\\ (" ^ e ^ "))" 
  
let func v  e =
  "(lam " ^ v ^ "\\ (" ^ e ^ "))" 
  
let newc v  e =
  "(new " ^ (String.uncapitalize_ascii v) ^ "\\ (" ^ e ^ "))" 

let cpair p1 p2 =
  "(pr (" ^ p1 ^ ") (" ^ p2 ^ "))"

                             
let pair e1 e2 =
  "(pair arobase (" ^ e1 ^ ") arobase (" ^ e2 ^ "))"

let to_pr =
  let rec aux = function
      [] -> ""
    | [a] -> a
    | [a;b] -> cpair a b
    | a::tl -> cpair a (aux tl)
  in aux

let to_pair =
  let rec aux = function
      [] -> ""
    | [a] -> a
    | [a;b] -> pair a b
    | a::tl -> pair a (aux tl)
  in aux
                                          
let type_constr name l =
  "(" ^ name
  ^ (if List.length l > 0 then
       " (" ^ (to_pr l) ^ ")"
     else "")
  ^ ")"

let appc head args =
  (specials head)
  (*^"toto"^(string_of_int (List.length args))*)
    ^(if args = [] then "" else " arobase (" ^ (to_pair args) ^ ")")
             
let app head args =
  (specials head)
  (*^"toto"^(string_of_int (List.length args))*)
  ^(if args = [] then ""
    else (to_separated_list ~first:true " arobase " args)
         )
      
let appv head args =
  (specials head) 
  ^ (to_separated_list ~first:true " " args)
