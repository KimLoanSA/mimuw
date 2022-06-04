#!/usr/bin/env bash

echo -e "building..."
swipl --goal=verify --stand_alone=true -o verify -c ma406058.pl
echo -e "Building done!"

threads=2


echo "================================"
echo -e "Running validation tests..."
echo "================================"


echo -e "Running test: 0 args"
echo -e "--------------------------------\n"

./verify

echo -e "\n--------------------------------\n\n"


echo -e "Running test: 1 args"
echo -e "--------------------------------\n"

./verify $threads

echo -e "\n--------------------------------\n\n"


echo -e "Running test: 3 args"
echo -e "--------------------------------\n"

./verify $threads "lol1" "lol"

echo -e "\n--------------------------------\n\n"


echo -e "Running test: 0 threads"
echo -e "--------------------------------\n"

./verify 0 "lol"

echo -e "\n--------------------------------\n\n"


echo -e "Running test: -1 threads"
echo -e "--------------------------------\n"

./verify -1 "lol"

echo -e "\n--------------------------------\n\n"


echo -e "Running test: invalid file"
echo -e "--------------------------------\n"

./verify 2 "invalid_file.txt"

echo -e "\n--------------------------------\n\n"

echo "================================"
echo -e "Running file tests..."
echo "================================"


for test in programs/*; do

  echo -e "Running test: $test"
  echo -e "--------------------------------\n"

  ./verify $threads $test

  echo -e "\n--------------------------------"
  echo -e "Done!\n\n"

done
