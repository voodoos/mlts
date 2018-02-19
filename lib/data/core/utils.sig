sig utils.
accum_sig lists.

% Utility to only extract first result :
type first prop -> prop.

% makes a list L of all solutions of a goal G of one output :
type alli (A -> prop) -> list A -> prop.

type for int -> (int -> prop) -> prop.


% FOR ELPI
type int_to_string int -> string.
type size string -> int.
type substring string -> int -> int -> string.