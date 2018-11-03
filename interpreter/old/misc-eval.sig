sig misc-eval.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Evaluation by rewriting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
type non_val       tm -> prop.
type redex         tm -> prop.
type reduce        tm -> tm -> prop.
type context       tm -> (tm -> tm) -> tm -> prop.
type evalc         tm -> tm -> prop.

type testc        int -> tm -> prop.
type mixeval       tm -> tm -> prop.



type ftrans, phi       tm -> tm -> prop.

type adm                              (tm -> tm) -> tm.
type phi', ftrans', admred, red1, red   tm -> tm -> prop.
