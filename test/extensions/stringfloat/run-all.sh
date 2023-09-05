#!/bin/bash

echo "Test - String 1"
rm a.out &> /dev/null
./compile.sh test-string.mc &> /dev/null
./a.out

echo "Test - String 2"
rm a.out &> /dev/null
./compile.sh test-string2.mc &> /dev/null
./a.out

echo "Test - String 3: strcat"
rm a.out &> /dev/null
./compile.sh test-strcat.mc strcat.mc &> /dev/null
./a.out

echo "Test - Float 1"
rm a.out &> /dev/null
./compile.sh test-float.mc &> /dev/null
./a.out

echo "Test - Float 2: PI"
rm a.out &> /dev/null
./compile.sh test-pi.mc &> /dev/null
./a.out

echo "Test - Float 3: Promotion"
rm a.out &> /dev/null
./compile.sh test-promotion.mc &> /dev/null
./a.out

echo "Test - Float 4: Increment and Decrement"
rm a.out &> /dev/null
./compile.sh test-incr-decr.mc &> /dev/null
./a.out


rm a.out &> /dev/null
