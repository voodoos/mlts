{

  open Lexing
  open MltsParser

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "true",     CONST_BOOL(true);
	"false",    CONST_BOOL(false);
	"let",      LET;
	"rec",	    REC;
	"in",	    IN;
	"if",       IF;
	"of",	    OF;
	"then",     THEN;
	"else",     ELSE;
	"match",    MATCH;
	"with",     WITH;
	"type",	    TYPE;
	"nab",	    NA;
	"fun",	    FUN;
	"new", 	    NEW;
	"begin",    BEGIN;
	"end", 	    END;
	"list",	    LIST;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)


  exception Error of string * Lexing.position

  let lexing_error lexbuf =
      let invalid_input = String.make 1 (Lexing.lexeme_char lexbuf 0) in
      raise (Error ("Invalid input \"" ^ invalid_input ^ "\"", lexbuf.Lexing.lex_curr_p))

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphadigit = alpha | '_' | '\'' | digit

let blank = [' ' '\t']+
let newline = ('\r'* '\n')

let lowercaseIdent = ['a'-'z' '_'] alphadigit*
let uppercaseIdent = ['A'-'Z'] alphadigit*  

rule token = parse
  | newline
      { Lexing.new_line lexbuf; token lexbuf }
  | blank+
      { token lexbuf }
  | "(*"
      { comment lexbuf; token lexbuf }
  | digit+
      { CONST_INT (int_of_string (lexeme lexbuf)) }
  | lowercaseIdent
      { id_or_keyword (lexeme lexbuf) }
  | uppercaseIdent
      { UPIDENT (lexeme lexbuf) }
  | "("
      { BEGIN }
  | ")"
      { END }
  | "["
      { LBRACK }
  | "]"
      { RBRACK }
  | ";;"
      { DSEMI }
  | "::"
      { DCOLON }
  | "."
      { DOT }
  | ","
      { COMMA }
  | "\\"
      { BACKSLASH }  
  | "->"
      { ARROW }
  | "=>"
      { DARROW }
  | "+"
      { PLUS }
  | "-"
      { MINUS }
  | "*"
      { STAR }
  | "="
      { EQUAL }
  | "!="
      { NEQ }
  | "<"
      { LT }
  | "<="
      { LE }
  | "&&"
      { AND }
  | "||"
      { OR }
  | "|"
      { VBAR }
  | "@"
      { AT }
  | _
      { lexing_error lexbuf  }
  | eof
      { EOF }

and comment = parse
  | "(*"
      { comment lexbuf; comment lexbuf }
  | "*)"
      { () }
  | _
      { comment lexbuf }
  | eof
      { failwith "Unterminated comment" }
