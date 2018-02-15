sig uprogs.

accum_sig spy, control, lists.
accum_sig eval.
accum_sig typing.

type prog     string -> tm -> prop.
type test     int -> tm -> prop.
type testall  prop.
type report   tm -> tm -> prop.


%%%% Data type for first-order terms over the signature {a/0, b/0, f/1, g/2}

type a, b, h, g    tm.
type hh            tm -> tm.
type gg            tm -> tm -> tm.
type folterm       ty.

type dp            tm -> tm -> tm.
type dpair         tm.
type dpty          ty.

%%%% Data type for unification problems (quantified or not)

type qunif  ty.
type unif   ty.

type coerce tm.
type coe    tm -> tm.
type some   (tm -> tm) -> tm.
type som    (tm -> tm) -> tm.
