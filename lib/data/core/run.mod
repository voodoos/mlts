module run.
% This is the harness for testing and examples.

accumulate spy, control, lists.
accumulate eval.
accumulate typing.
%accumulate datatypes.
accumulate progs_gen.
accumulate json.


%testall :- (test N V, term_to_string N Str, print "Value for ", print Str, print ": ",
%                      term_to_string V Vstr, print Vstr, print "\n", fail);
%           (prog Name Prog, print "Type of ", print Name, print " is: ", typeof Prog Type,
%	                    term_to_string Type Tscr, print Tscr, print "\n", fail).
%testall.

all P L :-
    all_aux P [] L.

all_aux P Acc L :-
    (P N, not (member N Acc), !),
    all_aux P (N::Acc) L.

all_aux _ L L.

run S :- prog Name Prog, eval Prog V,
            term_to_string Prog SProg,
            term_to_string V SV,
	    json_new J,
	    json_add_string "value" SV J J',
	    json_add_string "prog" SProg J' J'',
	    json_add_string "name" Name J'' S.


run_all Json :-
	json_new J,
	all (run) List,
	json_array_from_json_list List Jarray,
	json_add_val "output" Jarray J J',
	string_of_json J' Json.
	
