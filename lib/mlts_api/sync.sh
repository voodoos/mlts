#!/usr/bin/env bash


cp ../../../code/parser/arity.ml .
cp ../../../code/parser/mltsAst.ml .
cp ../../../code/parser/mltsLexer.mll .
cp ../../../code/parser/mltsParser.mly .
cp ../../../code/parser/lpStrings.ml .
cp ../../../code/parser/astTools.ml .
cp ../../../code/parser/translator.ml .

sed -i '' 's/\".*(@).*\"/arobase/g' lpStrings.ml
