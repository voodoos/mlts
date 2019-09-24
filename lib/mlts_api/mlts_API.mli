exception Error of string * int * int
val parse_and_translate : string -> 
    (PrologAst.clause list * (string * int) list)

val pp_prog : Format.formatter -> PrologAst.clause list -> unit

(* Parses, translates and print the resulting AST *)
val prologify : string -> ( string * (string * int) list)
 
