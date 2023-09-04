#!/bin/bash

echo "Test 1"
rm a.out &> /dev/null
./compile.sh test-pleq.mc &> /dev/null
./a.out

echo "Test 2"
rm a.out &> /dev/null
./compile.sh test-preinc.mc &> /dev/null
./a.out

echo "Fail 1"
rm a.out &> /dev/null
./compile.sh fail-prepost.mc

echo "Fail 2"
rm a.out &> /dev/null
./compile.sh fail-prepost2.mc

echo "Fail 3"
rm a.out &> /dev/null
./compile.sh fail-prepost3.mc

rm a.out &> /dev/null
