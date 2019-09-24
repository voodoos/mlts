// Compiled on https://pegjs.org/online
{
  function strip(t) {
    if(t.substring(0, 2) == "t_") {
    	return t.substring(2);
    }
    return t;
  }
}

Expression
  = _ "arr" arr:(SimpleExpr SimpleExpr) 
  		{ return (arr[0] + " -> " + arr[1])}
  / _ "bigarr" arr:(SimpleExpr SimpleExpr) 
  		{ return (arr[0] + " => " + arr[1])}
  / _ "pair" p:(SimpleExpr SimpleExpr) 
  		{ return ("pair " + p[0] + " " + p[1])}
  
  / _ i:TIdent e:(SimpleExpr+)
  		{return e.join(" ") + " " + i }
  / _ s: SimpleExpr { return s }
        
SimpleExpr
  = _ "(" expr:Expression ")" 
  		{ return "(" + expr +")" }
    / _ i:Ident 
  		{return i}
    
Ident
  = t:([_0-9a-z]i+) { return (strip(text())) }

TIdent
  = t:("t_" [_0-9a-z]i+) { return (strip(text())) }

_ "whitespace"
  = [ \t\n\r]* {return ""}
