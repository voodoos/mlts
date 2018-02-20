%{

  open MltsAst

%}

%token <int> CONST_INT
%token PLUS MINUS STAR
%token <bool> CONST_BOOL
%token AND OR
%token EQUAL NEQ LT LE
%token <string> IDENT
%token <string> UPIDENT
%token BEGIN END
%token LBRACK RBRACK
%token IF THEN ELSE
%token FUN 
%token MATCH WITH VBAR ARROW DARROW
%token DSEMI
%token DCOLON
%token DOT BACKSLASH COMMA
%token TYPE OF LET REC IN
%token EOF
%token NA NEW

%nonassoc IN
%nonassoc TYPE
%nonassoc LET
%nonassoc THEN
%nonassoc ELSE
%left VBAR
%left COMMA
%right OR
%right AND
%right DCOLON
%left LE LT EQUAL NEQ
%left PLUS MINUS
%left STAR
%left IDENT
%nonassoc BEGIN END
%nonassoc DOT BACKSLASH

%start main
%type <MltsAst.prog> main

%%

main:
| mi = module_items EOF			{ mi }
;

module_items:
| /* empty */				{ [] }
| module_item module_items 		{ $1 :: $2 }
;

module_item:
| definition; DSEMI			{ IDef($1) }
| expr	; DSEMI				{ IExpr($1) }
;

definition:
| LET; REC; b = let_binding		{ DLetrec(b) }
| TYPE; n = typeconstr_name; EQUAL; ltc = nonempty_list(constr_decl)
					{ DType(n, ltc) }
;

constr_decl:
/*| VBAR; nc = constr_name		{ Simple(nc) }*/
| VBAR; nc = constr_name; OF; tya = arityp_expr
					{ Of(nc, tya) }
;

let_binding:
| v = value_name; p = list(value_name); EQUAL; e = expr	{ LBVal(v, p, e) }
;

simple_expr:
| constant				{ EConst($1) }
| BEGIN; e = expr; END			{ e }
| value_path				{ EVal($1) }
| BEGIN; e1 = expr; COMMA; e2 = expr; END
					{ EPair(e1, e2) }
| c = constr_path;			{ EConstr(c, []) }
| c = constr_path;
    BEGIN;
    l = separated_nonempty_list(COMMA, expr);
    END;				{ EConstr(c, l) }

expr:
| simple_expr				{ $1 }
| LET; b = let_binding; IN; e = expr
					{ ELetin(b, e) }
| LET; REC; b = let_binding; IN; e = expr
					{ ELetRecin(b, e) }
| IF; e1 = expr; THEN e2 = expr; ELSE; e3 = expr
      	   	      	   	       	{ EIf(e1, e2, e3) }
| MATCH; e = expr; WITH; pm = nonempty_list(rule)
					{ EMatch(e, pm) }
| FUN; i = value_name; ARROW; e = expr
					{ EFun(i, e) }
| NEW; i = constr_name; IN; e = expr
					{ ENew(i, e) }
| expr; nonempty_list(argument)		{ EApp($1, $2) }
| expr; infix_op; expr			{ EInfix($1, $2, $3) }
| e1 = expr; DCOLON; e2 = expr		{ EInfix(e1, ListCons, e2) }
| i = value_name; BACKSLASH; e = expr	{ EBind(i, e) }
;

arityp_expr:
| typeconstr_name			{ Cons($1), 0 }
| tya1 = arityp_expr;
  STAR; tya2 = arityp_expr			{ let ty1, a1 = tya1
   	   	  			  and ty2, a2 = tya2   in
  	       				      Sum(tya1, tya2), max a1 a2 }
|  tya1 = arityp_expr;
   DARROW;  tya2 = arityp_expr		{ let ty1, a1 = tya1
   	   	  			  and ty2, a2 = tya2   in
   	   	  	 Arrow(tya1, tya2), 1 + (max a1 a2)  }
;

rule:
| VBAR; p = pattern; ARROW; e = expr	{ RSimple(p, e) }
| VBAR; NA; i = nonempty_list(constr_name);
  IN; BEGIN; p = pattern; ARROW; e = expr; END;
      	       	 	  	     	{ RNa(i, p, e) }
;

pattern:
| BEGIN; p = pattern; END;		{ p }
| constant				{ PConstant($1) }
| value_path				{ PVal($1) }
| i = value_name; BACKSLASH; p = pattern
					{ PBind(i, p) }
| v = value_path;
    l = nonempty_list(pattern)
      				        { PApp(v, l) }
| c = constr_path;
    BEGIN;
    l = separated_nonempty_list(COMMA, pattern);
    END;
					{ PConstr(c, l) }
| p1 = pattern; DCOLON; p2 = pattern	{ PListCons(p1, p2) }
| BEGIN; p1 = pattern; COMMA; p2 = pattern; END
  	      	       	      	   	{ PPair(p1, p2) }
| c = constr_path;			{ PConstr(c, []) }
;

constant:
| CONST_INT				{ Int($1) }
| CONST_BOOL				{ Bool($1) }
| LBRACK; RBRACK			{ EmptyList }
;

argument:
| simple_expr				{ $1 }
;

value_path:
| value_name				{ $1 }
| m = module_name; DOT; v = value_name	{ m ^ "." ^ v }
/* TODO, long modulepath */
module_path:
| module_name				{ $1 }
| m = module_name; DOT; mp = module_path
					{ m ^ "." ^ mp}
;

constr_path:
| constr_name				{ $1 }

value_name:
| i = IDENT				{ i }
;

module_name:
| i = UPIDENT				{ i }
;

constr_name:
| i = UPIDENT				{ i }
;

typeconstr_name:
| i = IDENT				{ i }
;

%inline infix_op:
| PLUS   { Add  }
| MINUS  { Minus  }
| EQUAL  { Equal   }
| STAR   { Mult }
| NEQ    { Neq  }
| LT     { Lt   }
| LE     { Le   }
| AND    { And  }
| OR     { Or   } 
;
