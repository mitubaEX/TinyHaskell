#!/bin/zsh

make
cd vm/ && make
cd ..

while true
do
  read str
  echo $str | ./tree
  echo $str | ./tree | ./vm/main
  echo $str
done
