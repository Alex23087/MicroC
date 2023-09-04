#!/bin/bash
CC="bin/microcc.exe"

ARGS=("$@")
ARGS_BC=$(echo $@ | sed s/.mc/.bc/g)

for arg in "${ARGS[@]}"; do
    arg_bc=$(echo $arg | sed s/.mc/.bc/)
    opam exec -- dune exec --root ../../.. $CC $arg
    mv a.bc $arg_bc
done

# Compile runtime support
/usr/lib/llvm14/bin/clang -emit-llvm -c "../../../bin/rt-support.c"

# Link modules
llvm-link rt-support.bc $ARGS_BC -o output.bc

# Compile
llc -filetype=obj output.bc
/usr/lib/llvm14/bin/clang output.o -o a.out

rm $ARGS_BC rt-support.bc output.bc output.o