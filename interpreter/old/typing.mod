module typing.

% typeof  A B :- announce (typeof A B).
% typematch  A B C :- announce (typematch A B C).
% typematchrulex  A B C :- announce (typematchrulex A B C).

typeof (M arobase N) A          :- typeof M (arr B A), typeof N B',
       (B = B', !; err_wrong_type N B B' (M arobase N), fail).
			     
typeof (cond P Q R) A     :- typeof P B, typeof Q A, typeof R A',
       ( B = bool, !; err_wrong_type P (bool) B (cond P Q R), fail),
       (A = A', !; err_wrong_type R A A' (cond P Q R), fail).

typeof (lam M) (arr A B)  :- pi x\ typeof x A => typeof (M x) B.
typeof (fixpt M) A        :- pi x\ typeof x A => typeof (M x) A',
       (A = A', !; err_wrong_type (M x) A A' (fixpt M), fail).
typeof (let M R) A        :- typeof M B, pi x\ typeof x B => typeof (R x) A.

typeof (cns X Y) (lst A)    :- typeof X A, typeof Y A',
       (A' = lst A, !; err_wrong_type Y (lst A') A' (cns X Y), fail).
			       
typeof (pr X Y)  (prty A B) :- typeof X A, typeof Y B.

typeof tt bool & typeof and (arr bool (arr bool bool)).
typeof ff bool & typeof or  (arr bool (arr bool bool)).
typeof equal (arr A (arr A' bool)).

typeof null  (lst A)             & typeof cons  (arr A (arr (lst A) (lst A))).
typeof car   (arr (lst A) A)     & typeof cdr   (arr (lst A) (lst A)).
typeof consp (arr (lst A) bool)  & typeof nullp (arr (lst A) bool).
%typeof pair  (arr A (arr B (prty A B))).

typeof (i I) int                        & typeof zerop   (arr int bool).
typeof greater (arr int (arr int bool)) & typeof minus   (arr int (arr int int)).
typeof sum     (arr int (arr int int))  & typeof times   (arr int (arr int int)).

%typeof app     (arr utm (arr utm utm)).

%%%% New features to MLTS

% Below, typing of new and nab allows for introducing a new nominal of any type.
typeof (new R) A     :- pi x\ typeof x B   => typeof (R x) A.
%typeof (abt R) utm   :- pi x\ typeof x utm => typeof (R x) utm.
%typeof (ab  R) utm   :- pi x\ typeof x utm => typeof (R x) utm.
%typeof (ap M N) utm  :- typeof M utm, typeof N utm.

typeof (match Exp Rules) B :- typeof Exp A, typematch A Rules B.

typematch A (R::Rs) B :- typematchrulex A R B, typematch A Rs B',
	  (B = B', !; err_wrong_type (Rs) B B' "matching", fail).
typematch A []      B.

typematchrulex A (Exp ==> Result) B :- typeof Exp A, typeof Result B.
typematchrulex A (nab   R) B :- pi x\ typeof x C                           => typematchrulex A (R x) B.
typematchrulex A (all   R) B :- pi x\ typeof x C                           => typematchrulex A (R x) B.
typematchrulex A (all'  R) B :- pi x\ (pi u\ typeof (x u) C :- typeof u D) => typematchrulex A (R x) B.
typematchrulex A (all'' R) B :- pi x\ (pi u\ pi v\ typeof (x u v) C :- typeof u D, typeof v E) => typematchrulex A (R x) B.
