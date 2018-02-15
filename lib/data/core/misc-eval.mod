module misc-eval.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%    Rewriting-in-context specification of evaluation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

non_val (_ arobase _) & non_val (fixpt _) & non_val (cond _ _ _).
non_val M :- special _ M.

redex F       :- special _ F.
redex (U arobase V) :- val U, val V.
redex (cond tt _ _) & redex (cond ff _ _) & redex (fixpt _).

reduce ((lam R) arobase N) (R N).
reduce (fixpt R) (R (fixpt R)).
reduce (cond tt L R) L.
reduce (cond ff L R) R.
reduce F (spec C F []) :- special C F.
reduce ((spec 1 Fun Args) arobase N) V :- eval_spec Fun (N::Args) V.
reduce ((spec C Fun Args) arobase N) (spec D Fun (N::Args)) :-  C > 1, D is C - 1.

context R (x\ x) R :- redex R.
context (cond M N P) (x\ cond (E x) N P) R :- non_val M, context M E R.
context (M arobase N)    (x\ (E x) arobase N)    R :- non_val M, context M E R.
context (V arobase M)    (x\ V arobase (E x))    R :- val V, non_val M, context M E R.

evalc V V :- val V.
evalc M V :- context M E R, reduce R N, evalc (E N) V.


% Fischer translation, first pass

ftrans (lam V) (lam k\ k arobase U) :- phi (lam V) U.
ftrans (M arobase N) (lam k\ P arobase (lam m\ Q arobase (lam n\ m arobase k arobase n))) :-
    ftrans M P, ftrans N Q.

phi (lam M) (lam k\ lam x\ (P x) arobase k) :-
    pi x\ pi y\ ftrans x (lam k\ k arobase y) => ftrans (M x) (P y).

% Fischer translation, second pass

ftrans' (lam V) (adm k\ k arobase U) :- phi' (lam V) U.
ftrans' (M arobase N) (adm k\ P arobase (adm m\ Q arobase (adm n\ m arobase k arobase n))) :-
    ftrans' M P, ftrans' N Q.

phi' (lam M) (adm k\ lam x\ (P x) arobase k) :-
    pi x\ pi y\ ftrans' x (adm k\ k arobase y) => ftrans' (M x) (P y).

admred ((adm R) arobase N) (R N)  &  admred (adm (x\ M arobase x)) M.

red1 M N :- admred M N.
red1 (M arobase N) (M' arobase N) & red1 (N arobase M) (N arobase M') :- red1 M M'.
red1 (adm R) (adm S)  & red1 (lam R) (lam S)  :- pi x\ red1 (R x) (S x).

red M N :- red1 M P, !, red P N.
red M M.




testc 1 V :- prog "fib" Fib, evalc (Fib arobase (i 8)) V.
testc 2 V :- prog "fib" Fib, prog "map" Map, 
             evalc (Map arobase Fib arobase (cons arobase (i 3) arobase (cons arobase (i 4) arobase null))) V.
testc 3 V :- prog "appnd" App, evalc (App arobase (cons arobase (i 1) arobase (cons arobase (i 5) arobase null)) arobase (cons arobase (i 2) arobase null)) V.

mixeval (lam R) (lam S) :- pi k\ val k => eval (R k) (S k).

%%%%  Old code saved here for a bit.

%type matchl     tm -> tm -> (tm -> tm -> tm) -> tm.
%type matcht     tm -> (tm -> tm -> tm) -> ((tm -> tm) -> tm) -> (tm -> tm) -> tm.

% prog "size" (fixpt size\ lam term\ 
%   matcht term (x\y\     sum arobase (i 1) arobase (sum arobase (size arobase x) arobase (size arobase y)))
%               (r\ na y\ sum arobase (i 1) arobase (size arobase (r y)))
%               (x\       (i 1))).

% prog "maptm0" (fixpt maptm\ lam fapp\ lam fabs\ lam fvar\ lam term\
%   matcht term (x\y\ fapp arobase (maptm arobase fapp arobase fabs arobase fvar arobase x) arobase (maptm arobase fapp arobase fabs arobase fvar arobase y))
%               (r\   fabs arobase (lam x\ maptm arobase fapp arobase fabs arobase fvar arobase (r x)))
%               (x\   fvar arobase x)).

% Obsolete constructors now that we have the more general
% match-with-rulexxxs constructions.  Remove eventually.

% eval (matchl E0 E1 E2) V :- eval E0 U,
%                             (U = null, eval E1 V ;
%                              U = (cns X L), eval (E2 X L) V).
% eval (matcht E0 E1 E2 E3) V :- eval E0 U,
%                               (U = (ap M N), eval (E1 M N) V ;
%                                U = (ab R),   eval (E2 R)   V ;
%                                nom U,        eval (E3 U)   V).


% Obsolete constructors now that we have the more general
% match-with-rulexxxs constructions.  Remove eventually.

% typeof (matchl E0 E1 E2) A :- typeof E0 (lst B), typeof E1 A, 
%                               pi x\ pi y\ typeof x B =>  typeof y (lst B) => typeof (E2 x y) A.

% typeof (matcht E0 E1 E2 E3) A :- typeof E0 utm,
%                                  (pi x\  pi y\ typeof x utm => typeof y     utm  => typeof (E1 x y) A),
%                                  (pi r\ (pi Y\ typeof Y utm => typeof (r Y) utm) => typeof (E2 r) A),
%                                  (pi x\        typeof x utm                      => typeof (E3 x) A).

%% The match' was intended to match on an abstracted term (tm -> tm)
%% instead of just tm.  For now, I leave this out since it can the
%% effect is easily achieved without it.

%type match'        (tm -> tm) -> list rulexxx -> tm.
%type matching'     (tm -> tm) -> list rulexxx -> tm -> o.
%type applymatch'   list tm -> (tm -> tm) -> rulexxx -> tm -> o.
%type ===>          (tm -> tm) -> tm -> rulexxx.
%infixr  ===> 5.

% eval (match' Exp Rules) V :- eval (abt Exp) (ab U), matching' U Rules V.
% matching' Val (R::Rs) V :- applymatch' [] Val R V; matching' Val Rs V.

% applymatch' Ns Exp (all  R) V :- sigma X\ applymatch' Ns Exp (R X) V.
% applymatch' Ns Exp (all' R) V :- sigma X\ applymatch' Ns Exp (R X) V.
% applymatch' Ns Exp (nab  R) V :- pi x\ copy N x => eval x N => applymatch' [N|Ns] Exp (R x) V.
% applymatch' Ns Exp (Exp' ===> Result) V :- (pi x\ copy x x => copy (Exp x) (Exp' x)), foreach nom Ns, eval Result V.

