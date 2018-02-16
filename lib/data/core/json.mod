module json.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Hidden signature of the module %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type string_of_json_array (list json_val) -> string -> prop.
type string_of_json_array_aux (list json_val) -> string -> prop.
type string_of_json_val json_val -> string -> prop.

type string_of_json_aux json -> string -> prop.

type quoted string -> string -> prop.


%%%%%%%%%%%%%%%%%%%%%
%% String escaping %%
%%%%%%%%%%%%%%%%%%%%%

type escape_aux string -> int -> string -> prop.

escape S S :-
       Size is ((size S) - 1),
       escape_aux S Size S'.
       
escape_aux S I "" :- I < 0, !.
escape_aux S I S' :-
	   I >= 0,
	   Char is substring S I 1,
	   I' is I - 1,
	   escape_aux S I' S'',
	   if (Char = "\""; Char = "\\")
	      (S' is S'' ^ "\\" ^ Char)
	      (S' is S'' ^ Char).


%%%%%%%%%%%%%%%%%%%%
%%% Json magik : %%%
%%%%%%%%%%%%%%%%%%%%

string_of_json_val (v_string Str) S :- quoted Str S.
string_of_json_val (v_int I) S :- S is (int_to_string I).
string_of_json_val (v_object J) S :- string_of_json J S.
string_of_json_val (v_array A) S :- string_of_json_array A S.
string_of_json_val (v_true) "true".
string_of_json_val (v_false) "false".
string_of_json_val (v_null) "null".

string_of_json_array A S :-
    string_of_json_array_aux A S',
    S is "[" ^ S' ^ "]".

string_of_json_array_aux [] "".
string_of_json_array_aux (V::[]) S :-
    string_of_json_val V S, !.
string_of_json_array_aux (V::TL) S :-
    string_of_json_val V SV,
    string_of_json_array_aux TL STL,
    S is SV ^ ", " ^ STL.


string_of_json J S :-
    string_of_json_aux J SJ,
    S is "{ " ^ SJ ^ " }".
    
string_of_json_aux (object []) "".
string_of_json_aux (object ((kv K V)::[])) S :-
    string_of_json_val V SV,
    quoted K SK,
    S is SK ^ ": " ^ SV, !.
string_of_json_aux (object((kv K V)::TL)) S :-
    string_of_json_val V SV, quoted K SK,
    string_of_json_aux (object TL) STL,
    S is SK ^ ": " ^ SV ^ ", " ^ STL.

quoted S S' :- S' is "\"" ^ S ^ "\"".

json_new (object([])).

json_add_val	K V (object(L)) (object((kv K V)::L)).
json_add_string K SV (object(L)) (object((kv K (v_string SV))::L)) :-
		escape SV SV'.
json_add_int	K IV (object(L)) (object((kv K (v_int IV))::L)).

json_array_from_string_list [] (v_array []).
json_array_from_string_list (S::TL) (v_array ((v_string S')::A)) :-
  escape S S',
  json_array_from_string_list TL (v_array A).

json_array_from_json_list [] (v_array []).
json_array_from_json_list (S::TL) (v_array ((v_object S)::A)) :-
  json_array_from_json_list TL (v_array A).

json_test (object((kv "toto" (v_int 3))
	  	      ::(kv "tata" (v_array ((v_true)
		      	    	   	    ::v_false
					    ::[]))))).

 test S :- S is (int_to_string 3).
