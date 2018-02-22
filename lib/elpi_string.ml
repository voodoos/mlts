exception Query_failed

(* Tools for priting Elpi results 
 * These results have the type Elpi_API.Data.solution 
 *
 * type solution = {
 *    arg_names : int StrMap.t;
 *    assignments : term array;
 *    constraints : syntactic_constraints;
 *    custom_constraints : custom_constraints;
 *}
 *
 *)
let string_of_arg_names sm =
  Elpi_API.Data.StrMap.fold
    (fun k v acc -> ("("^k^": "^(string_of_int v)^")") ^ acc)
    sm ""

let string_of_assignments ass =
  Array.fold_left
    (fun acc term ->
      Elpi_API.Pp.term (Format.str_formatter) term;
      let str = Format.flush_str_formatter () in
      (* LP strings are surrounded by quotes, we remove them *)
       acc ^ str)
    "" ass

let string_of_constraints cons =
  Elpi_API.Pp.constraints (Format.str_formatter) cons;
  Format.flush_str_formatter ()
                             
let string_of_cconstraints cons =
  Elpi_API.Pp.custom_constraints (Format.str_formatter) cons;
  Format.flush_str_formatter ()
    
let string_of_sol (s : Elpi_API.Data.solution) =
  "Arg names : " ^ (string_of_arg_names (s.arg_names))
  ^ "\nAssignments : " ^ (string_of_assignments (s.assignments))
  ^ "\nConstraints : " ^ (string_of_constraints (s.constraints))
  ^ "\nCustom constraints : "
  ^ (string_of_cconstraints (s.custom_constraints))
  ^ "\n"

let loop_printer f (out : Elpi_API.Execute.outcome) =
  print_string ("\nIter "^ (string_of_float f) ^ ":\n");
  match out with
  | Success(data) -> print_string (string_of_sol data)
  | NoMoreSteps -> print_string "Finished (no more).\n"
  | Failure -> print_string "Finished (fail).\n"
