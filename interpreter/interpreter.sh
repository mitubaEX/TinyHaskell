#!/bin/zsh

make
cd vm/ && make
cd ..

while true
do
  read str
  echo $str | ./compiler
  echo $str | ./compiler | ./vm/main
  echo $str
done
