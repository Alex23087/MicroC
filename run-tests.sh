#!/bin/bash

make build

# Run tests that succeed
for fl in $(ls test/samples/test*.mc | grep -v ex7)

# Run tests that fail
# for fl in $(cat testfails.txt)
do
    f=$(echo $fl | sed s/mc$/out/)
    TESTFILE=$(echo $f | sed s/out$/mc/)
    echo $TESTFILE
    # read -rsp $'Press any key to continue...\n' -n 1
    make -s compile $TESTFILE 2> /dev/null; ./a.out > /dev/null
    #dune exec _build/default/test/codegen_test.exe $f 1> /dev/null
    rm a.out
done