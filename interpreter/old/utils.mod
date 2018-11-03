module utils.

first Prop :- Prop, !.

type all_aux (A -> prop) -> list A -> list A -> prop.

alli P L :-
    all_aux P [] L.

all_aux P Acc L :-
    (P N, not (member N Acc), !),
    all_aux P (N::Acc) L.

all_aux _ L L.


for 0 P :- P 0.
for I P :- P I, I2 is I - 1, for I2 P.

