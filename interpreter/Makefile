ELPI=elpi
RLWRAP=rlwrap

.PHONY: all run test
all: test

run:
	${RLWRAP} ${ELPI} src/run.elpi -exec "run N P V T."

test:
	${ELPI} -test src/tests.mod
