#!/bin/bash

make build

# Run tests that succeed
for fl in $(ls test/samples | grep .out | grep -Fvf testfails.txt | sed s/.out$//)

# Run tests that fail
# for fl in $(cat testfails.txt)
do
    f=test/samples/$fl.out
    TESTFILE=$(echo $f | sed s/out$/mc/)
    echo $TESTFILE
    # read -rsp $'Press any key to continue...\n' -n 1
    make -s compile $TESTFILE 2> /dev/null; ./a.out | diff $f -
    #dune exec _build/default/test/codegen_test.exe $f 1> /dev/null
    rm a.out
done