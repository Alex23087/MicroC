#!/bin/bash

fl=$(cat testfails.txt | head -1)
echo $fl

f=test/samples/$fl.out
TESTFILE=$(echo $f | sed s/out$/mc/)
make compile $TESTFILE && ./a.out | diff $f - && rm a.out