#!/bin/sh

for i in ./testData/*.txt
do
  echo "testData is $i:"
  ./main < $i
  echo
done
