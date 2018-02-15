let parse_and_translate mlts_prog =
  let tokens = Lexing.from_string mlts_prog in
  let p = MltsParser.main MltsLexer.token tokens in

  let prog, _, _, _ = Translator.toLPString p in
  prog
