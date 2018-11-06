exception Error of string * int * int

let ints_of_pos (pos : Lexing.position) =
  pos.pos_lnum, pos.pos_cnum - pos.pos_bol
                                 
let string_of_pos l c =
  ("Line " ^ (string_of_int l)
   ^ ", column "^ (string_of_int c) ^ ": ")

let parse_and_translate mlts_prog =
  let tokens = Lexing.from_string mlts_prog in
  try
    let p = MltsParser.main MltsLexer.token tokens in

    Translator.mlts_to_prolog p
  with Translator.TranslatorError(s, popt)
       -> let l, c = match popt with
              None -> 0, 0
            | Some(pos) -> ints_of_pos pos in
          raise (Error("Translation error : " ^ s, l, c))
     | MltsLexer.Error(s, pos)
       -> let l, c = ints_of_pos pos in
          raise (Error((string_of_pos l c) ^ "Lexing error : " ^ s, l, c))
     | MltsParser.Error ->
        let l, c = ints_of_pos (tokens.Lexing.lex_start_p) in
        raise (Error((string_of_pos l c) ^ "Parsing error.", l, c))

let pp_prog = PrologPrinter.pp_prog

let prologify prog =
  let prolog = parse_and_translate prog in
  let f = pp_prog (Format.str_formatter) prolog in
  Format.flush_str_formatter f

  (*
let _ =
  let c  = open_in "tests.mlts" in
  let lb = Lexing.from_channel c in
  let p  = MltsParser.main MltsLexer.token lb in
  close_in c;

  Format.printf "%a@."
    pp_prog (Translator.mlts_to_prolog p)
   
   *)
   
