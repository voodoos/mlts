open Format

open PrologAst

let pp_global_name ppf s = fprintf ppf "%s" s
let pp_local_name ppf (s, i) =
  if false (*i = 0*)
  then fprintf ppf "%s" s
  else fprintf ppf "%s_%d" s i

let pp_name ppf = function
  | Global n -> pp_global_name ppf n
  | Local n -> pp_local_name ppf n

let with_parens printer ppf x =
  fprintf ppf "(@[<2>%a@])" printer x

let rec pp_ty ppf ty =
  pp_ty_arrow ppf ty
and pp_ty_arrow ppf = function
  | Arrow (left, right) ->
    fprintf ppf "%a -> %a"
      pp_ty_simple left
      pp_ty right
  | simple -> pp_ty_simple ppf simple
and pp_ty_simple ppf = function
  | Name n -> pp_global_name ppf n
  | (Arrow _) as above -> with_parens pp_ty ppf above

let pp_atom ppf n = pp_name ppf n

let pp_literal ppf = function
  | Int n -> fprintf ppf "%d" n
  | String s -> fprintf ppf "%S" s
  | Bool b -> fprintf ppf "b%b" b

let rec pp_term ppf t =
  fprintf ppf "@[<2>%a@]"
    pp_term_seq t
and pp_term_seq ppf = function
  | Seq [] -> fprintf ppf "true"
  | Seq [t] -> pp_term_seq ppf t
  | Seq (t::ts) ->
    fprintf ppf "%a, %a"
      pp_term_below_seq t
      pp_term_seq (Seq ts)
  | below -> pp_term_below_seq ppf below
and pp_term_below_seq ppf =
  let self = pp_term_below_seq in
  function
  | Abs (n, t) ->
    fprintf ppf "%a \\ %a"
      pp_local_name n
      self t
  | Hyp (t, u) ->
    fprintf ppf "%a => %a"
      pp_term_app t
      self u
  | Eq (t, u) ->
    fprintf ppf "%a = %a"
      pp_term_app t
      pp_term_app u
  | below ->
    pp_term_cons ppf below
and pp_term_cons ppf = function
  | Cons (t, u) ->
    fprintf ppf "%a :: %a"
      pp_term_app t
      pp_term_cons u
  | below ->
    pp_term_app ppf below
and pp_term_app ppf = function
  | App (head, args) ->
    let pp_sep ppf () = fprintf ppf " " in
    fprintf ppf "%a %a"
      pp_atom head
      (pp_print_list ~pp_sep pp_term_simple) args 
  | below ->
    pp_term_simple ppf below
and pp_term_simple ppf = function
  | App (atom, []) ->
    pp_atom ppf atom
  | Lit lit ->
    pp_literal ppf lit
  | List terms -> 
    let pp_sep ppf () = fprintf ppf ",@ " in
    fprintf ppf "[@[%a@]]"
      (pp_print_list ~pp_sep pp_term_app) terms
  | (Seq _ | Abs _ | Hyp _ | Eq _ | Cons _ | App (_, _::_)) as above ->
    with_parens pp_term ppf above

let pp_sort ppf = function
  | Kind -> fprintf ppf "kind"
  | Type -> fprintf ppf "type"

let pp_decl ppf {sort; name; ty} =
  fprintf ppf "%a %a @[%a@]"
    pp_sort sort
    pp_global_name name
    pp_ty ty

let pp_def ppf {name; args; body} =
  fprintf ppf "%a%t"
    pp_term_app (App (Global name, args))
    (fun ppf -> match body with
       | None -> ()
       | Some body ->
         fprintf ppf " :- @[%a@]" pp_term body
    )

let pp_clause ppf = function
  | Declaration decl ->
    fprintf ppf "@[%a@]."
      pp_decl decl
  | Definition def ->
    fprintf ppf "@[%a@]."
      pp_def def

let pp_prog ppf clauses =
  let pp_sep ppf () = fprintf ppf "@." in
  pp_print_list ~pp_sep pp_clause ppf clauses
