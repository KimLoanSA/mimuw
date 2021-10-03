#!/bin/bash

PASSED_TESTS=0
ALL_TESTS=0
RESOURCES_PATH=src/test/resources/request_resolver_test

function update_counters() {
  if diff ${RESOURCES_PATH}/out ${RESOURCES_PATH}/id_$1.out -Z >/dev/null 2>&1
  then
    echo -e -n "OK"
    echo -e "\tfor test: $2"
    ((PASSED_TESTS++))
  else
    echo -e -n "WA"
    echo -e "\tfor test: $2"
    echo -e "Got:"
    cat ${RESOURCES_PATH}/out
    echo -e "\nExpected:"
    cat ${RESOURCES_PATH}/id_$1.out
  fi
  ((ALL_TESTS++))
}

function run_test() {
  ./request_resolver_test $1 > ${RESOURCES_PATH}/out
  update_counters $1 "$2"
}

echo -e "=================================="
echo -e "CREATING REPORT FOR RESPONSE TESTS"
echo -e "=================================="

run_test 1 "(id: 1)"
run_test 2 "(id: 2)"
run_test 3 "(id: 3)"
run_test 4 "(id: 4)"
run_test 5 "(id: 5)"
run_test 6 "(id: 6)"
run_test 6 "(id: 7)"

echo -e "Passed: ${PASSED_TESTS} / ${ALL_TESTS}"
echo -e "==================================\n"

if [[ ${ALL_TESTS} -eq ${PASSED_TESTS} ]]
  then
    exit 0
  fi
  exit 1