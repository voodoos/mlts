% This is the harness for testing and examples.
module progs.
accumulate spy, control, lists.
accumulate eval.
accumulate typing.

% List here various functional programs.

prog "and" (lam x\ lam y\ (cond (equal arobase x arobase ff) ff
                                (cond (equal arobase y arobase ff) ff tt))).
prog "fib" (fixpt fib\ lam n\
  cond (zerop arobase n) (i 0) 
       (cond (equal arobase n arobase (i 1)) (i 1) 
             (sum arobase (fib arobase (minus arobase n arobase (i 1))) arobase (fib arobase (minus arobase n arobase (i 2)))))).

prog "map" (fixpt map\ lam f\ lam l\ 
  cond (nullp arobase l) null (cons arobase (f arobase (car arobase l)) arobase (map arobase f arobase (cdr arobase l)))).

prog "mem" (fixpt mem\ lam x\ lam l\ 
  cond (nullp arobase l) ff 
       (cond (and arobase (consp arobase l) arobase (equal arobase (car arobase l) arobase x)) tt 
             (mem arobase x arobase (cdr arobase l)))).

prog "appnd" (fixpt appnd\ lam l\ lam k\
  cond (nullp arobase l) k (cons arobase (car arobase l) arobase (appnd arobase (cdr arobase l) arobase k))).

prog "memm" (fixpt mem\ lam x\ lam l\ 
  match l [(null ==> ff),
           (all X\ all L\ (cns X L) ==> (cond (equal arobase x arobase X) tt (mem arobase x arobase L)))]).

prog "appndm" (fixpt appnd\ lam l\ lam k\
  match l [(null ==> k),
           (all X\ all L\ (cns X L) ==> (cons arobase X arobase (appnd arobase L arobase k)))]).

prog "join" (fixpt join\ lam l\ lam k\
  match l [(null ==> k),
           (all X\ all L\ (cns X L) ==> (cond (MEM arobase X arobase k) (join arobase L arobase k)
                                              (cons arobase X arobase (join arobase L arobase k))))]) :-
  prog "mem" MEM.

prog "rm" (fixpt rm\ lam x\ lam l\
  match l [(null ==> null),
         (all X\ all L\ (cns X L) ==> (cond (equal arobase x arobase X) (rm arobase x arobase L)
	                                            (cons arobase X arobase (rm arobase x arobase L))))]).
prog "rm1" (fixpt rm\ lam x\ lam l\
  match l [(null ==> null),
           (all X\ all L\ (cns X L) ==> (cond (equal arobase x arobase X) (rm arobase x arobase L)
			                       (cons arobase X arobase (rm arobase x arobase L))))]).
prog "id" (fixpt id\ lam term \
  match term [(all M\ all N\ ((ap M N) ==> (app arobase (id arobase M) arobase (id arobase N)))),
              (all' R\        ((ab R)  ==> (abt x\ (id arobase (R x))))),
	      (nab x\         (x       ==> x))]).

prog "size" (fixpt size\ lam term\ 
  match term [(all n\ all m\ ((ap n m) ==> (sum arobase (i 1) arobase (sum arobase (size arobase n) arobase (size arobase m))))),
              (all' r\        ((ab r)  ==> (sum arobase (i 1) arobase (new x\ size arobase (r x))))),
	      (nab x\         (x       ==> (i 1)))]).

%  let rec maptm fapp fabs fvar term = match term with
%        | (App m n) = (fapp (maptm fapp fabs fvar m) (maptm fapp fabs fvar n))
%        | (Abs r)   = (fabs (fun x -> (maptm fapp fabs fvar (r x))))
%        | (nab x\ x = (fvar arobase x)) ;;

prog "maptm" (fixpt maptm\ lam fapp\ lam fabs\ lam fvar\ lam term\
  match term [(all  m\ all n\ ((ap m n) ==> (fapp arobase (maptm arobase fapp arobase fabs arobase fvar arobase m) arobase (maptm arobase fapp arobase fabs arobase fvar arobase n)))),
              (all' r\        ((ab r)   ==> (fabs arobase (lam x\ maptm arobase fapp arobase fabs arobase fvar arobase (r x))))),
              (nab x\         ( x       ==> (fvar arobase x)))]).

% let size term  = maptm (fun n -> fun m -> 1 + n + m) (fun r -> 1 + na x\ r x) (fun n -> 1) term;;
prog "size1" (lam term\ Maptm arobase (lam n\ lam m\ sum arobase (i 1) arobase (sum arobase n arobase m))
                              arobase (lam r\ (sum arobase (i 1) arobase (new x\ r arobase x)))
			      arobase (lam n\ (i 1))
			      arobase term) :- prog "maptm" Maptm.

% let fv term  = maptm join (fun r -> na x\ rm x (r x)) (fun x -> [x]) term;;
prog "fv" (lam term\ Maptm arobase (lam n\ lam m\ Join arobase n arobase m)
                           arobase (lam r\ (new x\ (Rm arobase x arobase (r arobase x))))
			   arobase (lam x\ cons arobase x arobase null)
			   arobase term) :- prog "maptm" Maptm, prog "join" Join,  prog "rm" Rm.

% Return true if argument is an abstraction whose bound variable is not
% free in the scope.

% The following definition is wrong.  Formally, it can return both tt
% and ff when applied to (abt x\x): in the second match expression,
% the last two patterns overlap.  This can, of course, be fixed by
% adding Prolog's cut primitive to the definition of matching in
% eval.mod.

prog "vacp-wrong" (fixpt vacp\ lam term\ 
  match term [(all m\ all n\ ((ap m n) ==> ff)),
	      (nab x\        (x        ==> ff)),
              (all' r\       ((ab r)   ==>
	          (new x\ let (fixpt aux\ lam term\ match term
		     [(all  m\ all n\ ((ap m n) ==> (AND arobase (aux arobase m) arobase (aux arobase n)))),
		      (all' r\        ((ab r)   ==> new y\ (aux arobase (r y)))),
		                      (x        ==> ff),
		      (nab z\         (z        ==> tt))])
		      (aux\ (aux arobase (r x))))))])  :- prog "and" AND.

prog "vacp" (fixpt vacp\ lam term\
  match term [(all m\ all n\ ((ap m n) ==> ff)),
	      (nab x\        (x        ==> ff)),
              (all' r\       ((ab r)   ==>
	          (let (fixpt aux\ lam term\ match term
		     [(all'  m\ all' n\ ((ab x\ (ap (m x) (n x))) ==> (AND arobase (aux arobase (abt m)) arobase (aux arobase (abt n))))),
		      (all'' r\         ((ab x\ (ab y\ r x y))    ==> new y\ (aux arobase (abt x\ (r x y))))),
		      (                 ((ab x\ x)                ==> ff)),
		      (nab y\           ((ab x\ y)                ==> tt))])
		      (aux\ (aux arobase (abt r))))))])  :- prog "and" AND.

prog "let-test" (let (i 3) (x\ let (sum arobase x arobase x) (y\ let (sum arobase y arobase y) (w\ sum arobase w arobase w)))).

% let rec aux(var,body,term) = match (var,body) with
%      na x\   (x,(App (m x) (n x))) = (App (aux x m term) (aux x n term))
%    | na x\   (x,(Abs (r x))      ) = (Abs y\ aux x (r x y) term)
%    | na x\   (x,x                ) = term
%    | na x y\ (x,y                ) = y
%  in let objsub absterm t = na x\ aux(x,(absterm x),t);;

prog "aux_beta" 
     (fixpt aux\ lam var\ lam body\ lam term\ 
       match (cons arobase var arobase (cons arobase body arobase null))
          [(all'  m\ all' n\ nab x\ ((cns x (cns (ap (m x) (n x)) null)) ==> (app arobase (aux arobase x arobase (m x) arobase term) arobase (aux arobase x arobase (n x) arobase term)))),
           (all'' r\ nab  x\        ((cns x (cns (ab (r x))       null)) ==> (abt y\ (aux arobase x arobase (r x y) arobase term)))),
           (nab x\               ((cns x (cns x                   null)) ==> term)),
           (nab x\ nab y\        ((cns x (cns y                   null)) ==> y))]).

prog "beta" (lam absterm\ lam term\ match absterm [(all' r\ ((ab r) ==> (new x\ (AUX arobase x arobase (r x) arobase term))))]) :-
   prog "aux_beta" AUX.

% let rec objsub absterm t = match absterm with
%            (ab x\ App (m x) (n x)) t = (App (objsub m t) (objsub n t))
%    |       (ab x\ Abs (r x))       t = (Abs y\ objsub (x\ r x y) t)
%    |       (ab x\ x)               t = t
%    | na y\ (ab x\ y)               t = y

prog "bred" (fixpt bred\ lam aterm\ lam term\ match aterm 
     [(all' m\ all' n\  ((ab x\ ap (m x) (n x)) ==> (app arobase (bred arobase (abt m) arobase term) arobase (bred arobase (abt n) arobase term)))),
      (all'' r\         ((ab x\ ab (r x))       ==> (abt y\ bred arobase (abt x\ r x y) arobase term))),
                        ((ab x\ x)              ==> term),
      (nab y\           ((ab x\ y)              ==> y))]).

prog "cntx" (fixpt cntx\ lam pairs\ match pairs 
             [(all T\ all R\ nab n\ ((cns (pr n T) R) ==> (cntx arobase R))),
                                                (null ==> tt),
              (all P\                           (P    ==> ff))]).
% A simple function to check on backtracking in matching
prog "diag" (fixpt diag\ lam list\ match list
             [(all T\ all R\ ((cns T R) ==> (diag arobase R))),
                                  (null ==> tt),
              (all P\             (P    ==> ff))]).

prog "mapvar" (fixpt mapvar\ lam f\ lam t\ match t
      [(all  m\ all n\ (ap m n) ==> (app arobase (mapvar arobase f arobase m) arobase (mapvar arobase f arobase n))),
       (all' r\        (ab r)   ==> (abt x\ mapvar arobase f arobase (r x))),
       (nab  x\        (x       ==> (f arobase x)))]).

% let subst_tm sub = maptm App Abs (lookup sub) ;;

% let rec lookup sub = fn term => match term with na x\ x = 
%       let aux s = match s with 
%                       | Emp                 = x                   
%                       |       Subst x t sub = t
%                       | na y\ Subst y (t y) (sub y) = aux (sub y)
% 
%        in (aux sub);

prog "lookup" 
  (lam sub\ lam term\ match term [(nab x\ x ==>
       (let (fixpt aux\ lam s\ match s [(                  null                ==> x),
                                        (all  t\ all  sub\ (cns (pr x t) sub)  ==> t),
					(all p\ all sub\   (cns p sub)         ==> (aux arobase sub))])
            (aux \ (aux arobase sub))))]).

% let rec substtm sub term = mapvar (lookup sub) term;

prog "substtm" (lam sub\ lam term\ (MAPVAR arobase (LOOKUP arobase sub) arobase term)) :-
  prog "mapvar" MAPVAR, prog "lookup" LOOKUP.

test 1 V :- prog "fib" Fib, eval (Fib arobase (i 8)) V.
test 2 V :- prog "fib" Fib, prog "map" Map, eval (Map arobase Fib arobase (cons arobase (i 3) arobase (cons arobase (i 4) arobase null))) V.
test 3 V :- prog "appnd" App, eval (App arobase (cons arobase (i 1) arobase (cons arobase (i 5) arobase null)) arobase (cons arobase (i 2) arobase null)) V.
test 4 V :- prog "fib" Fib, eval (Fib arobase (i 12)) V.
test 5 V :- prog "memm" Mem, eval (Mem arobase (i 3) arobase (cons arobase (i 3) arobase (cons arobase (i 4) arobase null))) V.
test 6 V :- prog "memm" Mem, eval (Mem arobase (i 2) arobase (cons arobase (i 3) arobase (cons arobase (i 4) arobase null))) V.
test 7 V :- prog "appndm" App, eval (App arobase (cons arobase (i 1) arobase (cons arobase (i 5) arobase null)) arobase (cons arobase (i 2) arobase null)) V.
test 8 V :- eval (app arobase (abt x\x) arobase (abt x\x)) V.
test 9 V :- eval (abt x\ app arobase x arobase x) V.
test 10 V :- prog "size" Size, eval (Size arobase (app arobase (abt x\x) arobase (abt x\x))) V.
test 11 V :- prog "size" Size, eval (Size arobase (abt x\ app arobase x arobase x)) V.
test 12 V :- prog "size" Size, eval (Size arobase (abt y\ (app arobase (app arobase (abt x\x) arobase (abt x\y)) arobase (app arobase (abt x\y) arobase (abt x\x))))) V.
test 13 V :- prog "join" Join, eval (Join arobase (cons arobase (i 2) arobase (cons arobase (i 3) arobase (cons arobase (i 4) arobase null)))
                                          arobase (cons arobase (i 2) arobase (cons arobase (i 3) arobase (cons arobase (i 4) arobase null)))) V.
test 14 V :- prog "rm" Rm,  eval (Rm arobase (i 3) arobase (cons arobase (i 2) arobase (cons arobase (i 3) arobase (cons arobase (i 4) arobase null)))) V.
test 15 V :- prog "fv" Fv,  eval (Fv arobase (abt x\ app arobase x arobase x)) V.
test 16 V :- prog "fv" Fv,  eval (Fv arobase (abt x\ abt y\ app arobase y arobase (app arobase x arobase y))) V.
test 17 V :- prog "rm1" Rm, eval (Rm arobase (i 3) arobase (cons arobase (i 2) arobase (cons arobase (i 3) arobase (cons arobase (i 4) arobase null)))) V.
test 18 V :- eval (abt x\ match x [nab y\ (y ==> y)]) V.
test 19 V :- prog "id" Id, eval (Id arobase (abt x\ abt y\ y)) V.
test 20 V :- prog "id" Id, eval (Id arobase (app arobase (abt x\x) arobase (abt x\x))) V.
test 21 V :- prog "id" Id, eval (Id arobase (abt x\ (app arobase x arobase x))) V.
test 22 V :- prog "id" Id, eval (Id arobase (abt x\ abt y\ x)) V.
test 23 V :- eval (let (i 2) (x\ sum arobase x arobase x)) V.
test 24 V :- prog "let-test" Exp,    eval Exp V.
test 25 V :- prog "vacp-wrong" Vacp, eval (Vacp arobase (abt x\x)) V.
test 26 V :- prog "vacp-wrong" Vacp, eval (Vacp arobase (abt x\ abt y\ y)) V.
test 27 V :- prog "vacp-wrong" Vacp, eval (Vacp arobase (abt x\ app arobase x arobase x)) V.
test 28 V :- prog "and" And,   eval (cons arobase (And arobase tt arobase ff) arobase (cons arobase (And arobase tt arobase tt) arobase null)) V.
test 29 V :- prog "vacp" Vacp, eval (Vacp arobase (abt x\ x)) V.
test 30 V :- prog "vacp" Vacp, eval (Vacp arobase (abt x\ app arobase x arobase x)) V.
test 31 V :- prog "vacp" Vacp, eval (Vacp arobase (abt x\ abt y\ y)) V.
test 32 V :- prog "beta" Beta, eval (Beta arobase (abt x\x) arobase (abt x\x)) V.
test 33 V :- prog "beta" Beta, eval (Beta arobase (abt x\ app arobase x arobase x) arobase (abt x\x)) V.
test 34 V :- prog "beta" Beta, eval (Beta arobase (abt x\ abt y\ app arobase x arobase x) arobase (abt x\x)) V.
test 35 V :- prog "beta" Beta, eval (Beta arobase (abt x\ abt y\ app arobase x arobase y) arobase (abt x\x)) V.
test 36 V :- prog "beta" Beta, eval (Beta arobase (abt x\ abt y\ x) arobase (abt x\x)) V.
test 37 V :- prog "beta" Beta, eval (Beta arobase (abt x\ abt y\ y) arobase (abt x\x)) V.
test 38 V :- prog "aux_beta" Aux, eval (new x\ (Aux arobase x arobase (abt y\ x) arobase (abt x\x))) V.
test 39 V :- eval (new x\ match x [(nab y\ y ==> (app arobase y arobase y))]) V.  % Fails correctly
test 40 V :- eval (abt x\ match x [(nab y\ y ==> (app arobase x arobase y))]) V.  % can both x and y be the same nominal?
test 41 V :- eval (abt x\ match (ap x x) [(nab y\ (ap y y) ==> y)]) V. 
test 42 V :- eval (abt x\ match (ap x x) [(nab y\ nab z\ (ap y z) ==> y)]) V. % Fails correctly
test 43 V :- eval (abt x\ abt y\ match (ap x y) [(nab u\ nab v\ (ap u v) ==> (app arobase v arobase u))]) V.
test 44 V :- eval (abt x\ abt y\ match (ap x x) [(nab u\ nab v\ (ap u v) ==> (app arobase v arobase u))]) V. % Fails correctly
test 45 V :- prog "bred" Bred, eval (Bred arobase (abt x\x) arobase (abt x\x)) V.
test 46 V :- prog "bred" Bred, eval (Bred arobase (abt x\ app arobase x arobase x) arobase (abt x\x)) V.
test 47 V :- prog "bred" Bred, eval (Bred arobase (abt x\ abt y\ app arobase x arobase y) arobase (abt x\x)) V.
test 48 V :- prog "bred" Bred, eval (Bred arobase (abt x\ app arobase x arobase x) arobase (abt x\ app arobase x arobase x)) V.
test 50 V :- eval (pair arobase (i 2) arobase tt) V.
test 51 V :- prog "cntx" Cntx, eval (new x\ (Cntx arobase (cons arobase (pair arobase x arobase (i 1)) arobase null))) V.
test 52 V :- prog "cntx" Cntx, eval (new x\ (Cntx arobase (cons arobase (pair arobase x arobase x) arobase null))) V.
test 53 V :- eval (new x\ match (pair arobase x arobase x) [(all T\ nab x\ ((pr x T)  ==> tt))]) V. % Fail, good.
test 54 V :- eval (new x\ match (pair arobase x arobase x) [(nab y\ nab x\ ((pr x y)  ==> tt))]) V. % Fail, good.
test 55 V :- prog "diag" Diag, eval (Diag arobase (cons arobase (pair arobase (i 2) arobase (i 1)) arobase null)) V.
test 56 V :- eval (match (cons arobase (i 1) arobase null) [(all T\ all R\ ((cns T R) ==> tt)), (null ==> tt), (all P\ (P ==> ff))]) V.
test 57 V :- eval (match (cons arobase (i 1) arobase null) [(all T\ all R\ ((cns T R) ==> (i 3))), (null ==> (i 4)), (all P\ (P ==> (i 5)))]) V.
test 60 V :- prog "size1" Size, eval (Size arobase (app arobase (abt x\x) arobase (abt x\x))) V.
test 61 V :- prog "size1" Size, eval (Size arobase (abt x\ app arobase x arobase x)) V.
test 62 V :- prog "size1" Size, eval (Size arobase (abt y\ (app arobase (app arobase (abt x\x) arobase (abt x\y)) arobase (app arobase (abt x\y) arobase (abt x\x))))) V.
test 63 V :- prog "size1" Size, prog "beta" Beta, eval (Size arobase (Beta arobase (abt x\ (app arobase (app arobase x arobase x) arobase (app arobase x arobase x))) arobase (abt w\ app arobase w arobase w))) V.
test 64 V :- prog "fv" FV, 
             eval (FV arobase (abt x\ (app arobase (app arobase x arobase x) arobase (app arobase x arobase x)))) V.
test 65 V :- prog "beta" Beta, prog "fv" FV, 
             eval (FV arobase (Beta arobase (abt x\ (app arobase (app arobase x arobase x) arobase (app arobase x arobase x))) arobase (abt w\ app arobase w arobase w))) V.
test 66 V :- prog "fv" FV, 
             eval (abt x\ (FV arobase (app arobase (app arobase x arobase x) arobase (app arobase x arobase x)))) V.

% let rec mapvar f t = 
%   match t with (App m n) = (App (mapvar f m) (mapvar f n))
%              | (Abs r)   = (Abs x\ mapvar f (r x))
%              | na x\ x   = f x;
% 

test 67 V :- prog "substtm" SUBSTTM, 
             eval (new x\ abt y\ SUBSTTM arobase 
                                 (cons arobase (pair arobase x arobase y) arobase null) arobase 
                                 (app arobase x arobase (abt z\z))
                   ) V.
test 68 V :- prog "substtm" SUBSTTM, 
             eval (new x\ new y\ SUBSTTM arobase 
                                 (cons arobase (pair arobase x arobase (abt z\z)) arobase (cons arobase (pair arobase y arobase (abt w\w)) arobase null)) arobase 
                                 (app arobase x arobase (abt z\ app arobase z arobase y))
                   ) V.
test 69 V :- prog "substtm" SUBSTTM, 
             eval (new x\ new y\ SUBSTTM arobase 
                                 (cons arobase (pair arobase x arobase (abt z\z)) arobase (cons arobase (pair arobase y arobase (abt w\w)) arobase null)) arobase 
                                 (app arobase y arobase (abt z\ app arobase z arobase y))
                   ) V.
test 70 V :- prog "substtm" SUBSTTM, 
             eval (new x\ abt y\ SUBSTTM arobase 
                                 null arobase
                                 (app arobase y arobase (abt z\ app arobase z arobase y))
                   ) V.
test 71 V :- prog "substtm" SUBSTTM,            % Should fail
             eval (new x\ new y\ abt u \
 	            SUBSTTM arobase 
                    (cons arobase (pair arobase x arobase x) arobase (cons arobase (pair arobase y arobase (abt w\w)) arobase null)) arobase 
                    (app arobase x arobase (abt z\ app arobase z arobase y))
                   ) V.

testall :- (test N V, term_to_string N Str, print "Value for ", print Str, print ": ",
                      term_to_string V Vstr, print Vstr, print "\n", fail);
           (prog Name Prog, print "Type of ", print Name, print " is: ", typeof Prog Type,
	                    term_to_string Type Tscr, print Tscr, print "\n", fail).
testall.
