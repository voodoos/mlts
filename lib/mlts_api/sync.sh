#!/usr/bin/env bash


cp ../../../code/parser/arity.ml .
cp ../../../code/parser/mltsAst.ml .
cp ../../../code/parser/mltsLexer.mll .
cp ../../../code/parser/mltsParser.mly .
cp ../../../code/parser/lpStrings.ml .
cp ../../../code/parser/astTools.ml .
cp ../../../code/parser/translator.ml .
cp ../../../code/parser/datatypes_translation.ml .
cp ../../../code/parser/datatypes.ml .
cp ../../../code/parser/datatypes_typeof.ml .
cp ../../../code/parser/datatypes_eval.ml .
cp ../../../code/parser/datatypes_sig.ml .

sed -i '' 's/@/arobase/g' lpStrings.ml
sed -i '' 's/nil/[]/g' datatypes_eval.ml
