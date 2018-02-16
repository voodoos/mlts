module eval.

% eval A B  :- announce (eval A B).
% applymatch A B C D E :- announce (applymatch A B C D E).
% copy   A B   :- announce (copy A B).

fixbug T :- term_to_string T _.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Copy clauses allowing one to substitution into program
%%%% expressions.

copy (X arobase Y)      (U arobase V)      :- copy X U, copy Y V.
copy (cond X Y Z) (cond U V W) :- copy X U, copy Y V, copy Z W.
copy (spec X Y Z) (spec X Y W) :- mappred copy Z W.

copy (match X Y) (match U V) :- copy X U, mappred copyrulexxxx Y V.

copy (lam R)   (lam S)   :- pi x\ copy x x => copy (R x) (S x).
copy (fixpt R) (fixpt S) :- pi x\ copy x x => copy (R x) (S x).
copy (new R)   (new S)   :- pi x\ copy x x => copy (R x) (S x).
copy (let X R) (let U S) :- copy X U, pi x\ copy x x => copy (R x) (S x).

copyrulexxxx (X ==> Y) (U ==> V) :- copy X U, copy Y V.

copyrulexxxx (nab R) (nab S) :- pi x\ copy x x => copyrulexxxx (R x) (S x).
copyrulexxxx (all R) (all S) :- pi x\ copy x x => copyrulexxxx (R x) (S x).

copyrulexxxx (all'  R) (all'  S) :- pi x\ (pi X\ pi U\ copy (x X) (x U) :- copy X U) => copyrulexxxx (R x) (S x).
copyrulexxxx (all'' R) (all'' S) :- pi x\ (pi X\ pi Y\ pi U\ pi V\ copy (x X Y) (x U V) :- copy X U, copy Y V) => copyrulexxxx (R x) (S x).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pin G :- pi x\ nom x => (G x). % Use this to introduce a nominal

val (lam _).                % Functions (for HO programming)
val N  :- nom N.            % Nominals are values

% Big-step semantic specification
eval V            V :- val V.
eval (M arobase N)      V :- eval M F, eval N U, apply F U V.
eval (fixpt R)    V :- eval (R (fixpt R)) V.
eval (let M R)    V :- eval M U, eval (R U) V.
eval (cond C L R) V :- eval C B, if (B = tt) (eval L V) (eval R V).
eval (new E)      V :- fixbug E, pin x\ eval (E x) V.

apply (lam R) U   V :- eval (R U) V.

% Treatment for built-in special functions.
val    (spec _ _ _).
eval F (spec I F [])        :- special I F.
apply  (spec 1 Fun Args) U V :- eval_spec Fun (U::Args) V.
apply  (spec C Fun Args) U (spec D Fun (U::Args)) :- C > 1, D is C - 1.

% Every special has arity 0 and copies to itself.
copy F F :- special _ F.

% Allow backtracking during matching? The following placement of
% Prolog's cut does not allow backtracking past a successful match.

eval (match  Exp Rules) V :- eval Exp U, matching  U Rules Out, !, eval Out V.

matching Val (R::Rs) V :- applymatch [] [] Val R V    ; matching Val Rs V.

applymatch Ns Vs Exp (all   R) V :- sigma X\ applymatch Ns (arity0 X::Vs) Exp (R X) V.
applymatch Ns Vs Exp (all'  R) V :- sigma X\ applymatch Ns (arity1 X::Vs) Exp (R X) V.
applymatch Ns Vs Exp (all'' R) V :- sigma X\ applymatch Ns (arity2 X::Vs) Exp (R X) V.
applymatch Ns Vs Exp (nab   R) V :- pi x\    applymatch [(copy N x)|Ns] Vs Exp (R x) V.
applymatch Ns Vs Exp (Exp' ==> Result) V :-
  ((pi N\ pi X\ (copy N X) :- member (copy N X) Ns) => (pi N\ copy N N :- nom N) => (copy Exp Exp')),
  mappred (A\N\ sigma X\ ((copy N X) = A)) Ns Noms,
  (foreach nom Noms),
  (distinct Noms),
  ((pi N\ pi X\ ((copy N X) :- member (copy X N) Ns)) => (pi N\ copy N N :- nom N) => (copy Result V)),
  (foreach (not_supported Noms) Vs).

not_supported Noms (arity0 T) :- notsup Noms T.
not_supported Noms (arity1 T) :- pi x\ copy x x => notsup Noms (T x).
not_supported Noms (arity2 T) :- pi x\ copy x x => pi y\ copy y y => notsup Noms (T x y).

notsup Noms T :- (pi N\ copy N N :- nom N, not(member N Noms)) => copy T T.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%    The untyped datatype
val (ab _) & val (ap _ _).  % Object-level untyped lambda-terms

copy (abt R)   (abt S)   :- pi x\ copy x x => copy (R x) (S x).
copy (ab R)    (ab S)    :- pi x\ copy x x => copy (R x) (S x).
copy (ap  X Y) (ap  U V) :- copy X U, copy Y V.

special 2 app.
eval_spec app     (U::V::[]) (ap V U).

eval  (abt R) (ab S)    :- fixbug R, pin x\ eval (R x) (S x).
apply (ab  R) T (R T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%    The integer datatype
val (i _).
copy (i N) (i N).

special 1 zerop  & special 2 minus & special 2 sum   & special 2 times &
special 2 greater.

eval_spec minus   ((i N)::(i M)::[]) (i V) :- V is M - N.
eval_spec sum     ((i N)::(i M)::[]) (i V) :- V is M + N.
eval_spec times   ((i N)::(i M)::[]) (i V) :- V is M * N.
eval_spec zerop   ((i N)::[]) V        :- if (N = 0) (V = tt) (V = ff).
eval_spec greater ((i N)::(i M)::[]) V :- if (M > N) (V = tt) (V = ff).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%    The bool datatype
val tt.
val ff.
copy ff ff.
copy tt tt.
special 2 or     & special 2 and   & special 2 equal.
eval_spec and     (C::B::[]) V :- if (B = ff) (V = ff) (if (C = ff) (V = ff) (V = tt)).
eval_spec or      (C::B::[]) V :- if (B = tt) (V = tt) (if (C = tt) (V = tt) (V = ff)).
eval_spec equal   (C::B::[]) V :- if (B = C)  (V = tt) (V = ff).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%    The list datatype
val (cns _ _) & val null.
copy null null.
copy (cns X Y) (cns U V) :- copy X U, copy Y V.
eval_spec cons    (U::V::[]) (cns V U).
special 1 car    & special 1 cdr    & special 2 cons  & special 1 nullp & special 1 consp.
%   Deconstructors and predicates are not needed now that we have pattern matching.
eval_spec car     ((cns V U)::[]) V.
eval_spec cdr     ((cns V U)::[]) U.
eval_spec nullp   (U::[]) V :- if (U = null) (V = tt) (V = ff).
eval_spec consp   (U::[]) V :- if (U = null) (V = ff) (V = tt).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%    The pairing datatype
val (pr _ _).
special 2 pair.
eval_spec pair   (U::V::[]) (pr V U).
copy (pr X Y) (pr U V) :- copy X U, copy Y V.
