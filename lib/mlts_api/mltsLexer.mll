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
	"match",     MATCH;
	"with",     WITH;
	"type",	    TYPE;
	"nab",	    NA;
	"fun",		FUN;
	"new", 		NEW;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphadigit = alpha | '_' | '\'' | digit

let lowercaseIdent = ['a'-'z' '_'] alphadigit*
let uppercaseIdent = ['A'-'Z'] alphadigit*  

rule token = parse
  | ['\n' ' ' '\t' '\r']+
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
  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
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
