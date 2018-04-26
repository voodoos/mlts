#!/usr/bin/env bash

# Works on Macos
sed -i '' 's/o ->/prop ->/g' *.sig
sed -i '' 's/-> o/-> prop/g' *.sig
sed -i '' 's/ o\./ prop\./g' *.sig

sed -i '' 's/rule/rulex/g' *.sig
sed -i '' 's/rule/rulex/g' *.mod
sed -i '' 's/@/arobase/g' *.sig
sed -i '' 's/@/arobase/g' *.mod

sed -i '' 's/^external //' *.sig
sed -i '' '/%external/s/^/external /' *.sig
