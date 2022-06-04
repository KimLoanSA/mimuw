#!/bin/bash

PASSED_TESTS=0
ALL_TESTS=0
RESOURCES_PATH=src/test/resources/response_resolver_test

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
  ./response_resolver_test $1 > ${RESOURCES_PATH}/out
  update_counters $1 "$2"
}

echo -e "=================================="
echo -e "CREATING REPORT FOR RESPONSE TESTS"
echo -e "=================================="

run_test 1 "one part response 404 (id: 1)"
run_test 2 "big parts response 404 (id: 2)"
run_test 3 "small parts response 404 (id: 3)"
run_test 4 "one part response 200 (id: 4)"
run_test 5 "big parts response 200 (id: 5)"
run_test 6 "one part response 200, tricky cookies (id: 6)"
run_test 7 "one part response 200, tricky response size (id: 7)"
run_test 8 "one part response 200, chunked (id: 8)"
run_test 9 "big parts response 200, chunked (id: 9)"
run_test 10 "one part response 200, headers are non case sensitive (id: 10)"
run_test 11 "one part response 200, chunked header tricky (id: 11)"

echo -e "Passed: ${PASSED_TESTS} / ${ALL_TESTS}"
echo -e "==================================\n"

if [[ ${ALL_TESTS} -eq ${PASSED_TESTS} ]]
  then
    exit 0
  fi
  exit 1