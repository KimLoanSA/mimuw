#!/bin/bash

testsCounter=0
passedTestsCounter=0

echo -e "\nRunning tests..."

for test in $2/*.key
do
  echo -e "=========================================="
  echo "test: $test"

  ./$1/dcl $(cat $test) < ${test%key}a

  echo $? > code.exit

  if diff code.exit ${test%key}exit  >/dev/null 2>&1
  then
    echo -e "\nexit code OK"
    ((passedTestsCounter++))
  else
    echo -e "\nexit code WA"
  fi

  ((testsCounter++))

done

echo -e "=========================================="
echo -e "=========================================="
echo "Passed $passedTestsCounter / $testsCounter my tests"
echo -e "=========================================="
echo -e "=========================================="
echo -e "Python tests (n = 1 000):\n"

echo -e "=========================================="
echo -e "l = 30"
python3 $1/gen.py -n 1000 -l 30 imm ./$1/dcl ./$1/ref

echo -e "=========================================="
echo -e "l = 1 000"
python3 $1/gen.py -n 1000 -l 1000 imm ./$1/dcl ./$1/ref


echo -e "=========================================="
echo -e "n = 50, l = 1 000 000"
python3 $1/gen.py -n 100 -l 1000000 imm ./$1/dcl ./$1/ref
