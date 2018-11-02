%{

  open MltsAst


  (*let lmax =
      List.fold_left
	(fun acc (t,a) -> max acc a)
	0*)

  let unik =
    let c = ref 0 in
    let known = Hashtbl.create 100 in

    fun s ->
      try  Hashtbl.find known s
        with Not_found
	      -> let newname = c := !c + 1;
	           s^"_"^(string_of_int !c) in
	        Hashtbl.add known s newname; newname

  let n_type =
    let reserved = Hashtbl.create 5 in
    
    List.iter (fun (s, k) -> Hashtbl.add reserved s k)
      [ "int", "int";
        "bool", "bool" ] ;
    
    fun s ->
      try  Hashtbl.find reserved s
      with Not_found
        -> unik ("t_" ^ s)
	    
  let n_constr s =
      unik ("c_" ^ s)
      
  let n_val s =
      ("v_" ^ s)
  
%}

%token <int> CONST_INT
%token PLUS MINUS STAR
%token <bool> CONST_BOOL
%token AND OR
%token EQUAL NEQ LT LE
%token <string> IDENT
%token <string> UPIDENT
%token <string> STRING
%token BEGIN END
%token LBRACK RBRACK
%token IF THEN ELSE
%token FUN 
%token MATCH WITH VBAR ARROW
%token DSEMI
%token DCOLON
%token DOT BACKSLASH COMMA
%token TYPE OF DARROW LIST
%token LET REC IN
%token EOF
%token NA NEW
%token AT

%nonassoc ELSE
%right IN
%right VBAR


%right ARROW
%right DARROW
%right BACKSLASH

%right OR
%right AND
%left LE LT EQUAL NEQ

%right DCOLON

%left PLUS MINUS

%left STAR
%left LIST
/*
%nonassoc IDENT UPIDENT CONST_BOOL CONST_INT
*/
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
| definition; DSEMI			{ IDef($1, $startpos) }
| expr	; DSEMI				{ IExpr($1, $startpos) }
;

definition:
| LET;	 b = let_binding		{ DLet(b) }
| LET; REC; b = let_binding		{ DLetrec(b) }
| TYPE; n = typeconstr_name; EQUAL; ltc = nonempty_list(constr_decl)
					{ DType(n, ltc) }
;

constr_decl:
| VBAR; nc = constr_name		{ Simple(nc) }
| VBAR; nc = constr_name; OF; tya = arityp_expr
					{ Of(nc, tya) }
;

let_binding:
| v = value_name; p = list(value_name); EQUAL; e = expr	{ LBVal(v, p, e) }
;

constr_expr:
| c = constr_path                       { EConstr(c, []) }
;

trivial_expr_noconstr:
| constant                              { EConst($1) }
| value_path                            { EVal($1) }
;

simple_expr_noconstr:
| trivial_expr_noconstr                 { $1 }
| BEGIN; e = expr; END                  { e }
| BEGIN; e1 = expr; COMMA; e2 = expr; END
;
                                        { EPair(e1, e2) }
trivial_expr:
| trivial_expr_noconstr                 { $1 }
| constr_expr                           { $1 }
;

simple_expr:
| simple_expr_noconstr                  { $1 }
| constr_expr                           { $1 }
;

expr:
| simple_expr	                        { $1 }
| LET; b = let_binding; IN; e = expr
	%prec IN			{ ELetin(b, e) }
| LET; REC; b = let_binding; IN; e = expr
	%prec IN			{ ELetRecin(b, e) }
| IF; e1 = expr; THEN e2 = expr; ELSE; e3 = expr
      	   	      	   	       	{ EIf(e1, e2, e3) }
| MATCH; e = expr; WITH; pm = match_arms
  	     	   	      		{ EMatch(e, pm) }
| FUN; i = value_name; ARROW; e = expr
	%prec ARROW			{ EFun(i, e) }
| NEW; i = constr_name; IN; e = expr
	%prec IN			{ ENew(i, e) }
| se = simple_expr_noconstr;  AT;
  	a = nonempty_list(argument)
	                                { EBApp(se, a) }
| se = simple_expr_noconstr;
  	a = nonempty_list(argument)
	                                { EApp(se, a) }
| expr; infix_op; expr 			{ EInfix($1, $2, $3) }
| e1 = expr; DCOLON;
     e2 = expr				{ EInfix(e1, ListCons, e2) }
| i = constr_name; BACKSLASH; e = expr
      %prec BACKSLASH			{ EBind(i, e) }
      
| c = constr_path;
  args = constr_expr_args               { EConstr(c, args) }
;

constr_expr_args:
| s = trivial_expr                      { [s] }
| BEGIN; l = expr_list; END;		{ l }
;


expr_list:
| expr	{ [$1] }
| expr; COMMA; expr_list { $1::$3 }

arityp_expr:
| BEGIN; arityp_expr; END;		{ $2 }
| typeconstr_name			{ Cons($1), 0 }
| tya1 = arityp_expr;
  STAR; tya2 = arityp_expr		{ let _ty1, a1 = tya1
   	   	  			  and _ty2, a2 = tya2   in
  	       				      Sum(tya1, tya2), max a1 a2 }
					      
| tya1 = arityp_expr;
  ARROW; tya2 = arityp_expr		{ let _ty1, a1 = tya1
   	   	  			  and _ty2, a2 = tya2   in
  	       				      Arrow(tya1, tya2), max a1 a2 }
					      
|  tya1 = arityp_expr;
   DARROW;  tya2 = arityp_expr		{ let _ty1, a1 = tya1
   	   	  			  and _ty2, a2 = tya2   in
   	   	  	 		  Bind(tya1, tya2),
					  1 + (max a1 a2)  }
| tya = arityp_expr; LIST;		{ List(tya), (snd tya)  }
;

match_arms:
| VBAR; rule; match_arms { $2::$3 }
| VBAR; rule { [$2] }
;

rule:
| pe = pattern_arrow			{ let p, e = pe in RSimple(p, e) }
| NA; i = nonempty_list(constr_name);
  	IN; pe = pattern_arrow 
      	       	 	  	     	{  let p, e = pe in RNa(i, p, e) }
;

pattern_arrow:
| BEGIN; p = pattern_arrow; END; { p }
| p = pattern; ARROW; e = expr; { (p, e) }

pattern:
| sp = simple_pattern			{ sp }
| i = constr_name; BACKSLASH; p = pattern
					{ PBind(i, p) }
| v = value_path; AT;
    l = nonempty_list(simple_pattern)
      				        { PBApp(v, l) }
| v = value_path; 
    l = nonempty_list(simple_pattern)
      				        { PApp(v, l) }
					
| p1 = pattern; DCOLON; p2 = pattern	{ PListCons(p1, p2) }
| c = constr_path; l = constr_pat_args  { PConstr(c, l) }
;

constr_pat_args:
| p = trivial_pattern                   { [p] }
| BEGIN; l = separated_nonempty_list(COMMA, pattern); END;
					{ l }
;

simple_pattern:
| trivial_pattern                       { $1 }
| BEGIN; p = pattern; END		{ p }
| BEGIN; p1 = pattern; COMMA; p2 = pattern; END
;                                        { PPair(p1, p2) }

trivial_pattern:
| constr_path   			{ PConstr($1, []) }
| constant				{ PConstant($1) }
| value_path				{ PVal($1) }
;

constant:
| CONST_INT				{ Int($1) }
| CONST_BOOL				{ Bool($1) }
| STRING				{ String($1) }	
| LBRACK; RBRACK			{ EmptyList }
;

argument:
| simple_expr				{ $1 }
;

value_path:
| value_name				{ $1 }
| m = module_name; DOT; v = value_name	{ m ^ "." ^ v }

/* TODO, long modulepath 
module_path:
| module_name				{ $1 }
| m = module_name; DOT; mp = module_path
					{ m ^ "." ^ mp}
;
*/

constr_path:
| constr_name				{ $1 }

value_name:
| i = IDENT				{ n_val i }
;

module_name:
| i = UPIDENT				{ i }
;

constr_name:
| i = UPIDENT				{ n_constr i }
;

typeconstr_name:
| i = IDENT				{ n_type i }
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
