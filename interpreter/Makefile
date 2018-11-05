ELPI=elpi
RLWRAP=rlwrap

.PHONY: all run test
all: test

run:
	${RLWRAP} ${ELPI} src/run.elpi

test:
	${ELPI} -test src/tests.mod
