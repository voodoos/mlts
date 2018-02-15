% This is the harness for implementing the unification / type inference
% programs written in MLTS.

module uprogs.
accumulate spy, control, lists.
accumulate eval.
accumulate typing.

report Exp Val :- if (typeof Exp Ty)
                     (print "Type is ", term_to_string Ty Str, print Str, print ": ")
		     (print "Type error: "),
                  eval Exp Val.

% The standard testing predicate

testall :- (test N V, term_to_string N Str, print "Value for ", print Str, print ": ",
                      term_to_string V Vstr, print Vstr, print "\n", fail);
           (prog Name Prog, print "Type of ", print Name, print " is: ", typeof Prog Type,
	                    term_to_string Type Tscr, print Tscr, print "\n", fail).
testall.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Data type for first-order terms over the signature {a/0, b/0, h/1, g/2}
%%%% (ocaml) type folterm = A | B | H of folterm | G of folterm * folterm;;

val a & val b & val (hh _) & val (gg _ _).
special 1 h & special 2 g.
eval_spec h   (V::[])    (hh V).
eval_spec g   (U::V::[]) (gg V U).
copy a        a.
copy b        b.
copy (hh X)   (hh U)   :- copy X U.
copy (gg X Y) (gg U V) :- copy X U, copy Y V.

typeof a        folterm.
typeof b        folterm.
typeof (hh X)   folterm :- typeof X folterm.
typeof (gg X Y) folterm :- typeof X folterm, typeof Y folterm.
typeof h   (arr folterm folterm).
typeof g   (arr folterm (arr folterm folterm)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Data type for first-order unification problem (quantified and not quantified)
%%%% (ocaml) type unif = (pair (list (pair foterm foterm)) 
%%%%                           (list (pair foterm foterm)));;

%%%% No constructors are needed for this structure.

%%%% An unquantified unification problem is a pairs of two lists
%%%% of terms (Dpairs, Subst).
%%%% The list of pairs Dpairs is the list of disagreement pairs: the 
%%%% pairs of terms that should be unified.
%%%% The list of pairs Subst are the variable-term pairs that denote the 
%%%% substitution that solves the unification problem.

%%%% A quantified unification problem allows a number of existential
%%%% quantifiers around a unification problem.

%%%% coerce : (prty (lst (prty folterm folterm)) (lst (prty folterm folterm))) -> unif
%%%% some   : (foterm -> unif) -> unif.

val (coe _) & val (som _).
special 1 coerce.
eval_spec coerce (V::[]) (coe V).
eval (some R) (som S)    :- fixbug R, pin x\ eval (R x) (S x).

copy (coe  X) (coe  U) :- copy X U.
copy (som  X) (som  U) :- pi x\ copy x x => copy (X x) (U x).
copy (some X) (some U) :- pi x\ copy x x => copy (X x) (U x).

typeof coerce (arr (prty (lst (prty folterm folterm))
                         (lst (prty folterm folterm))) unif).
typeof (coe U)  unif :- typeof U (prty (lst (prty folterm folterm)) (lst (prty folterm folterm))).
typeof (som U)  unif :- pi x\ typeof x folterm => typeof (U x) unif.
typeof (some U) unif :- pi x\ typeof x folterm => typeof (U x) unif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%  The following specifications are borrowed from progs.mod

prog "and" (lam x\ lam y\ (cond (equal arobase x arobase ff) ff
                                (cond (equal arobase y arobase ff) ff tt))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%  The following specifications are new to this file

prog "eqfoterm" (fixpt eqfoterm\ lam x\ lam y\ 
     match (pair arobase x arobase y) [(nab x\ (pr x x) ==> tt),
                           (pr a a) ==> tt,
                           (pr b b) ==> tt,
                           (all U\ all V\ (pr (hh U) (hh V) ==> (eqfoterm arobase U arobase V))),
                           (all X\ all Y\ all U\ all V\ (pr (gg X Y) (gg U V) ==> 
                                (AND arobase (eqfoterm arobase X arobase U) arobase (eqfoterm arobase Y arobase V)))),
                           (all X\ (X ==> ff)) ]) :- prog "and" AND.

prog "simpl" (fixpt simpl\ lam unif\
     match unif [(null ==> null),
                (all'  L\ nab x\        (cns (pr x x) (L x))   ==> (simpl arobase (L x))),
                 (all'' L\ nab x\ nab y\ (cns (pr x y) (L x y)) ==> (simpl arobase (L x x))), % non-L-lambda
                 (all L\ (cns (pr a a) L) ==> (simpl arobase L)),
                 (all L\ (cns (pr b b) L) ==> (simpl arobase L)),
                 (all X\ all Y\ all L\ (cns (pr (hh X) (hh Y)) L) ==> 
                                  (simpl arobase (cons arobase (pair arobase X arobase Y) arobase L))),
                 (all X\ all Y\ all U\ all V\ all L\ (cns (pr (gg X Y) (gg U V)) L) ==> 
                                  (simpl arobase (cons arobase (pair arobase X arobase U) arobase (cons arobase (pair arobase Y arobase V) arobase L)))),
                 (all P\ all L\ (cns P L) ==> (cons arobase P arobase (simpl arobase L))) ]).

prog "simplify" (fixpt simplify\ lam unif\
      match unif [(all' R\        (som R)        ==> (some x\ simplify arobase (R x))),
                  (all  X\ all Y\ (coe (pr X Y)) ==> (coerce arobase (pair arobase (SIMPL arobase X) arobase Y)))]) :-
      prog "simpl" SIMPL.

prog "forsome" (fixpt forsome\ lam pred\ lam l\
                 match l [(null ==> ff),
                          (all X\ all L\ (cns X L) ==> (cond (pred arobase X) tt (forsome arobase pred arobase L))) ]).

prog "rigid?" (lam term\ match term [(nab x\ x ==> ff), (all Y\ Y ==> tt)]).

prog "rigidrigid?" 
     (lam dpairs\ FORSOME arobase 
         (lam p\ match p [(all X\ all Y\ (pr X Y) ==> (AND arobase (RIGID? arobase X) arobase (RIGID? arobase Y)))]) arobase
         dpairs)
     :- prog "forsome" FORSOME, prog "and" AND, prog "rigid?" RIGID?.                                    

prog "failure?" (fixpt failure?\ lam unif\ match unif 
                  [(all' R\ (som R) ==> new x\ failure? arobase (R x)),
                   (all  X\ all Y\ (coe (pr X Y)) ==> (RIGIDRIGID? arobase X))])
     :- prog "rigidrigid?" RIGIDRIGID?.


% Take the first disagreement pair.  
% It must be (var,var) or (var,term) or (term,var).
% Apply the intended substitution.  In the first case, use 
% beta-reduction (not beta-0).  In the other cases, apply the
% substitution explicitly.

prog "applyfirst" (fixpt applyfirst\ lam unif\
      match unif [(all' R\               (som R) ==> some x\ applyfirst arobase (R x)),
                  (all T\ all' L\ all' K\ nab v\ 
                      (coe (pr (cns (pr v T) (K v)) (L v))) ==> 
                      (cons arobase (pair arobase ((lam K) arobase T) arobase ((lam L) arobase T))))]).

% prog "mgu" 
%  (fixpt mgu\ lam unif\ let (SIMPLIFY arobase unif) simp\
%      cond (FAILURE? arobase simp) null
%           (match simp  ***



test 1 V :- prog "eqfoterm" EQ, report (EQ arobase a arobase a) V.
test 2 V :- prog "eqfoterm" EQ, report (EQ arobase a arobase b) V.
test 3 V :- prog "eqfoterm" EQ, report (EQ arobase a arobase (h arobase a)) V.
test 4 V :- prog "eqfoterm" EQ, report (EQ arobase (g arobase (g arobase a arobase b) arobase (h arobase (h arobase a)))
                                         arobase (g arobase (g arobase a arobase b) arobase (h arobase (h arobase a)))) V.
test 5 V :- prog "eqfoterm" EQ, report (EQ arobase (g arobase (g arobase a arobase b) arobase (h arobase (h arobase a)))
                                         arobase (g arobase (g arobase a arobase a) arobase (h arobase (h arobase a)))) V.
test 6 V :- prog "simplify" SIMPLIFY,
            report (SIMPLIFY arobase (coerce arobase (pair arobase (cons arobase (pair arobase a arobase a) arobase null) arobase null))) V.
test 7 V :- prog "simplify" SIMPLIFY,
            report (SIMPLIFY arobase (coerce arobase (pair arobase (cons arobase (pair arobase a arobase b) arobase null) arobase null))) V.
test 8 V :- prog "simplify" SIMPLIFY,
            report (SIMPLIFY arobase (some x\ coerce arobase (pair arobase (cons arobase (pair arobase x arobase x) arobase null) arobase null))) V.
test 9 V :- prog "simplify" SIMPLIFY,
            report (SIMPLIFY arobase (some x\ coerce arobase (pair arobase (cons arobase (pair arobase x arobase a) arobase 
                                                       (cons arobase (pair arobase a arobase a) arobase null)) arobase null))) V.
test 10 V :- prog "simpl" SIMPL,
             report (SIMPL arobase (cons arobase (pair arobase a arobase a) arobase null)) V.
test 11 V :- prog "simpl" SIMPL,
             report (SIMPL arobase (cons arobase (pair arobase a arobase b) arobase (cons arobase (pair arobase b arobase b) arobase null))) V.
test 12 V :- prog "simpl" SIMPL,
             report (SIMPL arobase (cons arobase (pair arobase (h arobase a)     arobase (h arobase a)) arobase
                           (cons arobase (pair arobase (g arobase a arobase a) arobase (g arobase a arobase b)) arobase null))) V.
test 13 V :- prog "simpl" SIMPL,
             report (SIMPL arobase (cons arobase (pair arobase (g arobase a arobase a) arobase (g arobase a arobase a)) arobase
                           (cons arobase (pair arobase (g arobase b arobase b) arobase (g arobase b arobase b)) arobase null))) V.
test 14 V :- prog "simplify" SIMPLIFY, 
             report (SIMPLIFY arobase (coerce arobase
                  (pair arobase (cons arobase (pair arobase a arobase a) arobase null) arobase null))) V.
test 15 V :- prog "simplify" SIMPLIFY, 
             report (SIMPLIFY arobase (some x\ coerce arobase
               (pair arobase (cons arobase (pair arobase x arobase a) arobase null) arobase null))) V.
test 16 V :-
 prog "simplify" SIMPLIFY, 
 report (SIMPLIFY arobase (some x\ coerce arobase
      (pair arobase (cons arobase (pair arobase (h arobase x) arobase (h arobase (h arobase a))) arobase null)
            arobase null))) V.
test 17 V :-
 prog "simplify" SIMPLIFY, 
 report (SIMPLIFY arobase (some x\ some y\ coerce arobase
          (pair arobase (cons arobase (pair arobase (g arobase x arobase x) arobase (g arobase (h arobase y) arobase (h arobase a))) arobase null) arobase null))) V.

test 18 V :-
 prog "simplify" SIMPLIFY, prog "failure?" FAILURE?,
 report (FAILURE? arobase (SIMPLIFY arobase (coerce arobase
          (pair arobase (cons arobase (pair arobase (g arobase a arobase a) arobase (g arobase (h arobase b) arobase (h arobase a))) arobase null) arobase null)))) V.

test 19 V :-
 prog "simplify" SIMPLIFY, prog "failure?" FAILURE?,
 report (FAILURE? arobase 
        (SIMPLIFY arobase (some x\ coerce arobase
          (pair arobase (cons arobase (pair arobase (g arobase x arobase x) arobase (g arobase (h arobase x) arobase (h arobase a))) arobase null) arobase null)))) 
        V.

test 20 V :-
 prog "simplify" SIMPLIFY, prog "failure?" FAILURE?,
 report (FAILURE? arobase (SIMPLIFY arobase (some x\ some y\ coerce arobase
          (pair arobase (cons arobase (pair arobase (g arobase x arobase x) arobase (g arobase (h arobase y) arobase (h arobase a))) arobase null) arobase null)))) V.

test 21 V :-
 prog "applyfirst" APPLYFIRST,
 report (APPLYFIRST arobase (some x\ some y\ coerce arobase
          (pair arobase (cons arobase (pair arobase x arobase (g arobase (h arobase y) arobase y)) arobase 
                  (cons arobase (pair arobase x arobase (g arobase x arobase b))  arobase null)) arobase null))) V.
