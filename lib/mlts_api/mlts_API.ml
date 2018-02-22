exception Error of string * int * int

let ints_of_pos (pos : Lexing.position) =
  pos.pos_lnum, pos.pos_cnum - pos.pos_bol
                                 
let string_of_pos l c =
  ("Line " ^ (string_of_int l)
   ^ ", column "^ (string_of_int c) ^ ": ")

exception Parse_error of string option * Lexing.position * Lexing.position
let parse parse_fun lexbuf =
  (* see the Menhir manual for the description of
     error messages support *)
  let open MenhirLib.General in
  let module Interp = MltsParser.MenhirInterpreter in
  let input = Interp.lexer_lexbuf_to_supplier MltsLexer.token lexbuf in
  let success prog = prog in
  let failure error_state =
    let env = match[@warning "-4"] error_state with
      | Interp.HandlingError env -> env
      | _ -> assert false in
    match Interp.stack env with
    | lazy Nil -> assert false
    | lazy (Cons (Interp.Element (state, _, start_pos, end_pos), _)) ->
      let message =
        match MltsParser_messages.message (Interp.number state) with
        | exception Not_found -> None
        | empty when String.trim empty = "" -> None
        | "<YOUR SYNTAX ERROR MESSAGE HERE>\n" -> None
        | error_message -> Some error_message
      in
      raise (Parse_error (message, start_pos, end_pos))
  in
  Interp.loop_handle success failure input
    (parse_fun lexbuf.Lexing.lex_curr_p)

let parse_and_translate mlts_prog =
  let tokens = Lexing.from_string mlts_prog in
  try
    let p = parse MltsParser.Incremental.main tokens in
    let prog, _, _, _ = Translator.toLPString p in
    prog
  with Translator.TranslatorError(s, pos)
       -> raise (Error("Translation error : " ^ s, 0, 0))
     | MltsLexer.Error(s, pos)
       -> let l, c = ints_of_pos pos in
          raise (Error((string_of_pos l c) ^ "Lexing error : " ^ s, l, c))
     | Parse_error (parser_message, start_pos, end_pos) ->
        let l, c = ints_of_pos start_pos in
        ignore end_pos;
        (* TODO: use both the start and end position of the error
           (they may span several characters and several lines) *)
        let message = Printf.sprintf "%sParsing error%s"
            (string_of_pos l c)
            (match parser_message with
             | None -> "."
             | Some str -> " : " ^ str) in
        raise (Error(message, l, c))
