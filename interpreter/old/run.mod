module run.
% This is the harness for testing and examples.

accumulate spy, control, lists.
accumulate eval.
accumulate typing.
accumulate utils.
accumulate datatypes.
accumulate progs_gen.
accumulate json.
accumulate errors.


%run_tests J4 :- prog Name Prog,
%       	    Size is size Name,
%	    if (Size > 4) (
%       	    Deb is substring Name 0 4,
%	    if (Deb = "test") (
 %      	    eval Prog V,
  % %         term_to_string Prog SProg,
   %         term_to_string V SV,
%	    json_new J,
%	    json_add_string "value" SV J J2,
%	    json_add_string "prog" SProg J2 J3,
%	    json_add_string "name" Name J3 J4
%	    ) (fail)
%	    ) (fail)
%	    .


%run_all Json :-
%	json_new J,
%	alli (run_tests) List,
%	json_array_from_json_list List Jarray,
%	json_add_val "output" Jarray J J',
%	string_of_json J' Json.
	

run_one Name Prog Value Type :-
	prog Name P, 
	term_to_string P Prog,
	if (typeof P T)
	   (term_to_string T Type)
  	   (Type is "Type error."),
	if (eval P V)
	   (term_to_string V Value)
	   (Value is "Evaluation failed.")
	.
