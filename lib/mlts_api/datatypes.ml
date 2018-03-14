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
                       "" l) ^ " a"^(string_of_int (maxl l))^")"
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
    | Bindl(_, _) ->  "Unexpected rank > 1 type."
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
  | Suml(l) -> List.flatten
                 (List.map
                 (base_level_arities)
                 l)

let print_bal b =
  let rec aux = function
    [] -> ""
    | at::tl -> (string_of_atypl at) ^ (aux tl)
  in ("("^(aux b)^")")
  

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

let rec get_last_bind (b, i) =
  match b with
  | Bindl(_, ((Bindl(_, _), _) as r)) -> get_last_bind r
  | Bindl(_, r) -> r
  | _ -> failwith "Not a bindl."
           
let make_args_from_int ?sym:(s = "X") i =
  List.init i (fun i -> s ^ (string_of_int i))
let make_args_from_list ?sym:(s = "X") l =
  make_args_from_int ~sym:s (List.length l)

 
(* Target :

val (c_Abs_3_v _).
copy (c_Abs_3_v X0) (c_Abs_3_v Y0) :- (pi x0\ copy x0 x0 => copy  (X0 x0) (Y0 x0)).
copy (c_Abs_3 X0) (c_Abs_3 Y0) :- (pi x0\ copy x0 x0 => copy  (X0 x0) (Y0 x0)).
eval (c_Abs_3 X0) (c_Abs_3_v Y0) :- fixbug X0, (pin x0\  eval  (X0 x0) (Y0 x0)).
val (c_App_2_v _).
copy (c_App_2_v (pr X0 X1)) (c_App_2_v (pr Y0 Y1)) :- copy X0 Y0, copy X1 Y1.
special 1 c_App_2.
eval_spec c_App_2 (X0::nil) (c_App_2_v X0).
typeof (c_Abs_3 X0) t_tm_1 :- ((pi x0\ typeof x0 t_tm_1 => typeof (X0 x0) t_tm_1)).
typeof (c_Abs_3_v X0) t_tm_1 :- ((pi x0\ typeof x0 t_tm_1 => typeof (X0 x0) t_tm_1)).

typeof c_App_2 (arr (prty t_tm_1 t_tm_1) t_tm_1).
typeof (c_App_2_v (pr X0 X1)) t_tm_1 :- (typeof X0 (t_tm_1)), (typeof X1 (t_tm_1)).

type t_tm_1 ty.
type c_Abs_3 (tm -> tm) -> tm.
type c_Abs_3_v (tm -> tm) -> tm.
type t_tm_1 ty.
type c_App_2 tm.
type c_App_2_v tm -> tm. *)
