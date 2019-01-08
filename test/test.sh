#!/bin/zsh

make
cd vm && make
cd ..

cat ./test/test.txt | while read i
do
  if [ "$i" != "" ]
  then
    echo "testData is \"$i\""
    echo $i | ./compiler
    echo $i | ./compiler | vm/main
    echo
  fi
done
