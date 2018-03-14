#!/usr/bin/env bash

ORIG=../../code
DEST=data/core

cp ${ORIG}/eval.mod ${DEST}
cp ${ORIG}/eval.sig ${DEST}

cp ${ORIG}/typing.mod ${DEST}
cp ${ORIG}/typing.sig ${DEST}

cp ${ORIG}/errors.mod ${DEST}
cp ${ORIG}/errors.sig ${DEST}
