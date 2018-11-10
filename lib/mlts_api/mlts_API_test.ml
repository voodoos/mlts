open Mlts_API

let () =
  let c  = open_in "tests.mlts" in
  let lb = Lexing.from_channel c in
  let p  = MltsParser.main MltsLexer.token lb in
  close_in c;

  Format.printf "%a@."
    pp_prog (fst (Translator.mlts_to_prolog p))
