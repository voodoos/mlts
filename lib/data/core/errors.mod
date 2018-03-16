module errors.


type errout     string -> o.
type type_error string -> o.
type eval_error string -> o.

errout S :- Mess is "<br>" ^ S, print Mess .
type_error S :- Mess is "Typing error: " ^ S, errout Mess .
eval_error S :- Mess is "Runtime error: " ^ S, errout Mess .

err_escaped N S :- term_to_string N Name,
	      	   term_to_string S Scope,
	      	   Mess is "Nominal " ^ Name ^
		   	   " escaped its scope in \""
			   ^ Scope ^ "\".",
	      	   eval_error Mess.


err_wrong_type N B C D :-
	       	   term_to_string N Name,
	      	   term_to_string B GoodTyp,
	      	   term_to_string C BadTyp,
	      	   term_to_string D Code,
		   Mess is "Expected " ^ Name
		   	   ^ " to be of type \"" ^ GoodTyp
			   ^ "\" got \"" ^ BadTyp
			   ^ "\" in " ^ Code ^ ".",
	      	   type_error Mess.
