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
  = _ "arr" arr:(Expression Expression) 
  		{ return (arr[0] + " -> " + arr[1])}
  / _ "bigarr" arr:(Expression Expression) 
  		{ return (arr[0] + " => " + arr[1])}
  / _ "pair" p:(Expression Expression) 
  		{ return ("pair " + p[0] + " " + p[1])}
  / _ i:Ident l:(Expression)
  		{ return (i + " " + l )}
  / _ "(" _ expr:Expression _ ")" 
  		{ return "(" + expr +")" }
  / _ i:Ident 
  		{return i}
  
Ident
  = t:([_0-9a-z]i+) { return (strip(text())) }

_ "whitespace"
  = [ \t\n\r]* {return ""}
