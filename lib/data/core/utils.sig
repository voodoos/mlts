sig utils.

% Utility to only extract first result :
type first prop -> prop.

% if Cond Then Else :
type if prop -> prop -> prop -> prop.

% check list membership :
type member A -> list A -> prop.

% makes a list L of all solutions of a goal G of one output :
type all (A -> prop) -> list A -> prop.

type for int -> (int -> prop) -> prop.
