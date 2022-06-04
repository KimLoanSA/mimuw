#!/bin/bash

PASSED_TESTS=0
ALL_TESTS=0
TESTHTTP_RAW_PATH=../solve/testhttp_raw

function update_counters() {
  if [[ $? == $1 ]]
  then
    ((PASSED_TESTS++))
  fi
  ((ALL_TESTS++))
}

./src/test/bash/program_arguments_validation_test.sh ${TESTHTTP_RAW_PATH}
update_counters 0

./src/test/bash/response_resolver_test.sh
update_counters 0

./src/test/bash/request_resolver_test.sh
update_counters 0

echo -e "\n=================================="
echo -e "Passed all: ${PASSED_TESTS} / ${ALL_TESTS}"
echo -e "==================================\n"

rm -f *.o