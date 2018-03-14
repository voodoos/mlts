module errors.


errout S :- Mess is "<br>" ^ S, print Mess .

err_escaped N S :- term_to_string N Name,
	      	   term_to_string S Scope,
	      	   Mess is "Nominal " ^ Name ^
		   	   " escaped its scope in \"" ^ Scope ^ "\".",
	      	   errout Mess.
