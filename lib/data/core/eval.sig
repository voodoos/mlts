sig eval.
accum_sig spy, control, lists.

% Converting to string forces normalization and fixes some endless cycling
type fixbug   A -> prop.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstract syntax
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
kind tm       type.
% Special forms: these have their own evaluation clauses
type arobase        tm -> tm -> tm.                % application
type lam      (tm -> tm) -> tm.              % function abstraction
type cond     tm -> tm -> tm -> tm.          % conditional
type fixpt    (tm -> tm) -> tm.              % recursive functions
type let      tm -> (tm -> tm) -> tm.        % let binding
type spec     int -> tm -> list tm -> tm.    % for treatment of specials
infixl        arobase 4.
% The basic constants of the language
type i                       int     -> tm.  % integers
type cns                    tm -> tm -> tm.  % list constructor (value space)
type and, or, ff, tt                    tm.  % boolean functions and constants
type cons, car, cdr, null, nullp, consp tm.  % list functions and constants
type greater, zerop, minus, sum, times  tm.  % integer functions and predicates
type equal                              tm.  % General equality

type pr           tm -> tm -> tm.
type pair         tm.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Big step evaluation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
type val           tm -> prop.
type eval          tm -> tm -> prop.
type apply         tm -> tm -> tm -> prop.
type eval_spec     tm -> list tm -> tm -> prop.
type special       int -> tm -> prop.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% New constructions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
type nom        tm -> prop.            % nominal terms
type new         (tm -> tm) -> tm.

%% The addition of untyped lambda-terms as expressions (abt/app) and
%% as values (ab/ap).
type app        tm.                 % expression 
type abt        (tm -> tm) -> tm.   % expression 
type ap         tm -> tm -> tm.     % value space
type ab         (tm -> tm) -> tm.   % value space

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Staging area to develop a more general pattern matching mechanism
%%%% with the possibility of using nominal abstractions.
kind rulexxxxx type.
type ==>            tm -> tm -> rulexxxxx.
infixr  ==> 5.
type nab            (tm -> rulexxxxx) -> rulexxxxx.
type all            (tm  -> rulexxxxx) -> rulexxxxx.               % For pattern variables that are not abstractions.
type all'           ((tm -> tm) -> rulexxxxx) -> rulexxxxx.        % For pattern variables that are abstractions.
type all''          ((tm -> tm -> tm) -> rulexxxxx) -> rulexxxxx.  % For pattern variables that are abstractions of two variables.

type match          tm -> list rulexxxxx -> tm.
type matching       tm -> list rulexxxxx -> tm -> prop.
type applymatch    list prop  -> list item -> tm -> rulexxxxx -> tm -> prop.

type not_supported  list tm -> item -> prop.
type notsup         list tm -> tm -> prop.

kind item           type.
type arity0         tm -> item.
type arity1         (tm -> tm) -> item.
type arity2         (tm -> tm -> tm) -> item.
type arity3         (tm -> tm -> tm -> tm) -> item.

type copy           tm -> tm -> prop.
type copyrulexxxxx       rulexxxxx -> rulexxxxx -> prop.
type pin            (tm -> prop) -> prop.
