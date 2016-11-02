set -e # stop on first error

INCLUDES="-I src -I src/plugins/aclite -I src/plugins/json"
IN=$1
OUT=test/test_$(basename "$IN")
TARGET=${OUT%.*}.native
EXE=$(basename $TARGET)

mkdir -p test
qtest -o $OUT extract $IN
ocamlbuild -cflags -warn-error,+26 -use-ocamlfind $INCLUDES -package oUnit -package qcheck $TARGET
./$EXE
