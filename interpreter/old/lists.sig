% 29 july 2014.
sig lists.

type append                    	list A -> list A -> list A -> prop.
type foldl                      (A -> B -> B -> prop) -> list A -> B -> B -> prop.
type foreach			(A -> prop) -> list A -> prop.
type forsome			(A -> prop) -> list A -> prop.
type length                     list A -> int -> prop.
type memb_and_rest              A -> list A -> list A -> prop.
type member                     A -> list A -> prop.
type reverse                    list A -> list A -> prop.
type distinct                   list A -> prop.
type mappred                    (A -> B -> prop) -> list A -> list B -> prop.


