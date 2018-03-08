open MltsAst

type atypl = typlist * int
and typlist =
  | Consl of constr_path
  | Suml of atypl list
  | Bindl of atypl * atypl
  | Arrowl of atypl * atypl
  | Listl of atypl

let maxl =
  List.fold_left
    (fun acc (_, i) -> max acc i)
    0 

let rec string_of_atypl = function
  | Consl(c), i -> "(" ^ c ^ " a" ^ (string_of_int i) ^ ")"
  | Suml(l), i -> "("
                  ^ (List.fold_left
                       (fun acc at
                        -> acc ^ " ("
                           ^ (string_of_atypl at) ^ ")"
                       )
                       "" l) ^ " a"^(string_of_int i)^")"
  | Bindl(at1, at2), i -> let r1 = string_of_atypl at1 and
                           r2 = string_of_atypl at2 in
                          "(" ^r1 ^ " => " ^ r2  ^ " a" ^ (string_of_int i) ^ ")"
  | Arrowl(at1, at2), i -> let r1 = string_of_atypl at1 and
                               r2 = string_of_atypl at2 in
                           "(" ^r1 ^ " -> " ^ r2  ^ " a" ^ (string_of_int i) ^ ")"
  | Listl(at1), i -> let r1 = string_of_atypl at1 in
                           "((" ^r1 ^ ") list a" ^ (string_of_int i) ^ ")"
                                                                                 
    
let rec atypl_of_aritytypexpr tname ate =
  let rec aux_suml_of_sum = function
    | (Sum((Sum(_,_), i1) as ate1, ate2), i) ->
       let r = aux_suml_of_sum ate1 in
       r @ [aux ate2]
    | (Sum(ate1, ((Sum(_,_), i2) as ate2)), i) ->
       let r = aux_suml_of_sum ate2 in
       (aux ate1)::r
    | (Sum(ate1, ate2), i) ->
       [aux ate1;
        aux ate2]
    | _ -> failwith "Arg, not a sum"
  and aux = function
    | (Cons(c), i) -> Consl(c), i
    | (Arrow(ate1, ate2), i)
      -> Arrowl(aux ate1, aux ate2), i
    | (Bind(ate1, ate2), i)
      -> Bindl(aux ate1,
               aux ate2), i
    | (List(ate), i)
      -> Listl(aux ate), i
    | (Sum(_, _), _) as s
      -> let l = aux_suml_of_sum s in Suml(l), maxl l
                        
  in
  let aty, i = aux ate in
  match aty with
    Consl("empty") -> Consl(tname), 0
  | _ -> Arrowl((aty, i), (Consl(tname), 0)), i



                                               
let lp_typ_of_atypl =
  let rec aux (ty, a)=
    match ty with
    | Consl(c) -> c
    | Arrowl(at1, at2)
      -> let r1, r2 = aux at1, aux at2 in
         "(arr " ^ r1 ^ " " ^ r2 ^ ")"
    | Listl(at)
      -> let r = aux at in
         "(lst " ^ r ^ ")"
    | Suml(l) -> aux_list l 
    | Bindl(_, _) -> failwith "Unexpected rank > 1 type."
    and aux_list = function
      [] -> ""
      | [a] -> aux a
      | [a; b] -> let r1, r2 = aux a, aux b in
                  "(prty " ^ r1 ^ " " ^ r2 ^")"
      | a::tl -> let r1, rtl = aux a, aux_list tl in
                 "(prty " ^ r1 ^ " " ^ rtl ^ ")"
  in aux 

let rec base_level_arities (ty, a) =
  match ty with
  | Consl(_) as t-> [t, 0]
  | Arrowl(_, _) as t -> [t, 0]
  | Listl(_) as t -> [t, 0]
  | Bindl(_, _) as t -> [t, a]
  | Suml(_) as t -> [t, 0](*List.flatten
                 (List.map
                 (base_level_arities)
                 l)*)

let get_args_typs  =
  let rec aux (typ, a)=
  (*print_string
    ("\ntoto : " ^ (string_of_atypl (typ, a)));*)
    match typ with
    | Consl(_) as t -> [t, a]
    | Listl(_) as t -> [t, a]
    | Arrowl(_, _) as t -> [t, a]
    | Suml(_) as t -> [t, a]
    | Bindl(ty1, (t2, i)) ->
       match t2 with
       | Bindl(_, _) ->
          ty1::(aux (t2, i))
       | _ -> [ty1]
      
  in
  fun (ty, a) ->
  match ty with
  | Bindl(ty1, ty2) -> aux (ty, a)
  | _ -> []
           
let make_args_from_int ?sym:(s = "X") i =
  List.init i (fun i -> s ^ (string_of_int i))
let make_args_from_list ?sym:(s = "X") l =
  make_args_from_int ~sym:s (List.length l)

 
