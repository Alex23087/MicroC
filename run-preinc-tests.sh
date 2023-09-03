#!/bin/bash

make build

for fl in $(ls test/extensions/prepost | grep .out | sed s/.out$//)
do
    f=test/extensions/prepost/$fl.out
    TESTFILE=$(echo $f | sed s/out$/mc/)
    echo $TESTFILE
    # read -rsp $'Press any key to continue...\n' -n 1
    make -s compile $TESTFILE 2> /dev/null; ./a.out | diff $f -
    #dune exec _build/default/test/codegen_test.exe $f 1> /dev/null
    rm a.out
done