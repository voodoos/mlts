sig json.
accum_sig utils.

% Internal representation of Json documents
% See https://www.json.org/ for full specification

kind json_val type.
type v_string	string	-> json_val.
type v_int 	int -> json_val.
type v_object   json -> json_val.
type v_array 	(list json_val) -> json_val.
type v_true 	json_val.
type v_false 	json_val.
type v_null	json_val.

kind keyval type.
type kv		string -> json_val -> keyval.

kind json type.
type object (list keyval) -> json.


type string_of_json 	  json -> string -> prop.

type json_new		  json -> prop.
type json_add_val	  string -> json_val -> json -> json -> prop.
type json_add_string	  string -> string -> json -> json -> prop.
type json_add_int	  string -> int -> json -> json -> prop.

type json_array_from_string_list
			  (list string) -> json_val -> prop.

type json_array_from_json_list
			  (list json) -> json_val -> prop.

type json_test 		  json -> prop.

type test string -> prop.
% escape " in string
type escape string -> string -> prop.
