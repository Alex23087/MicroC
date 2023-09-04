#!/bin/bash

rm a.out &> /dev/null
./compile.sh test-sepcomp.mc math.mc &> /dev/null
./a.out

rm a.out &> /dev/null
./compile.sh test-circ.mc &> /dev/null
./a.out

rm a.out &> /dev/null
./compile.sh test-sepcomp-double.mc first.mc second.mc &> /dev/null
./a.out


rm a.out &> /dev/null
