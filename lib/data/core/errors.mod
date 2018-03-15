module errors.


errout S :- Mess is "<br>" ^ S, print Mess .

err_escaped N S :- term_to_string N Name,
	      	   term_to_string S Scope,
	      	   Mess is "Nominal " ^ Name ^
		   	   " escaped its scope in \""
			   ^ Scope ^ "\".",
	      	   errout Mess.


err_wrong_type N B C D :-
	       	   term_to_string N Name,
	      	   term_to_string B GoodTyp,
	      	   term_to_string C BadTyp,
	      	   term_to_string D Code,
		   Mess is "Expected " ^ Name
		   	   ^ " to be of type \"" ^ GoodTyp
			   ^ "\" got \"" ^ BadTyp
			   ^ "\" in " ^ Code ^ ".",
	      	   errout Mess.