module utils.

first Prop :- Prop, !.

if B Then _ :- B, !, Then.
if _ _ Else :- Else.

member _ [] :- fail.
member A (A::TL) :- true, !.
member A (_::TL) :- member A TL.


type all_aux (A -> o) -> list A -> list A -> o.

all P L :-
    all_aux P [] L.

all_aux P Acc L :-
    (P N, not (member N Acc), !),
    all_aux P (N::Acc) L.

all_aux _ L L.


for 0 P :- P 0.
for I P :- P I, I2 is I - 1, for I2 P.

