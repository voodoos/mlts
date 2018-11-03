sig run.

accum_sig spy, control, lists.
accum_sig eval.
accum_sig typing.
accum_sig datatypes.
accum_sig progs_gen.
accum_sig json.

%type run_tests json -> prop.
%type run_all  string -> prop.
type run_one string -> string -> string -> string -> prop.
