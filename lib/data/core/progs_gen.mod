module progs_gen.



prog "join" ((fixpt join\ lam l\ lam k\ match (l) [( ((null) ==> (k))), (all H\ all Tl\ ((cns (H) (Tl)) ==> (cond (memm arobase (H) arobase (k)) (join arobase (Tl) arobase (k)) (cons arobase (H) arobase (join arobase (Tl) arobase (k))) )))])).
prog "rm" ((fixpt rm\ lam x\ lam l\ match (l) [( ((null) ==> (null))), (all H\ all Tl\ ((cns (H) (Tl)) ==> (cond (equal arobase (H) arobase (x)) (rm arobase (x) arobase (Tl)) (cons arobase (H) arobase (rm arobase (x) arobase (Tl))) )))])).
prog "maptm" ((fixpt maptm\ lam fapp\ lam fabs\ lam fvar\ lam term\ match (term) [(all M\ all N\ ((ap (M) (N)) ==> (fapp arobase (maptm arobase (fapp) arobase (fabs) arobase (fvar) arobase (M)) arobase (maptm arobase (fapp) arobase (fabs) arobase (fvar) arobase (N))))), (all' R\ ((ab (R)) ==> (fabs arobase ((lam x\ (maptm arobase (fapp) arobase (fabs) arobase (fvar) arobase (R (x)))))))), (nab x\ (( ((x ) ==> (fvar arobase (x))))))])).
prog "fv" ((fixpt fv\ lam term\ Maptm arobase ((lam n\ ((lam m\ (join arobase (n) arobase (m)))))) arobase ((lam r\ ((new x\ (Rm arobase (x) arobase (r arobase (x))))))) arobase ((lam x\ (cons arobase (x) arobase (null)))) arobase (term))) :- prog "maptm" Maptm, prog "rm" Rm.
 prog "test_1" (Fv arobase (abt ((x\ (app arobase (x) arobase (x)))))) :- prog "fv" Fv.
 prog "test_2" (Fv arobase (abt ((x\ (abt ((y\ (app arobase (y) arobase (app arobase (x) arobase (y)))))))))) :- prog "fv" Fv.
prog "test_3" (Fv arobase (abt ((x\ (app arobase (app arobase (x) arobase (x)) arobase (app arobase (x) arobase (x))))))) :- prog "fv" Fv.
prog "test_4" (abt ((x\ (Fv arobase (app arobase (app arobase (x) arobase (x)) arobase (app arobase (x) arobase (x))))))) :- prog "fv" Fv.
