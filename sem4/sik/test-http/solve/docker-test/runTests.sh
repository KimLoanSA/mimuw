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

#./src/test/bash/program_arguments_validation_test.sh ${TESTHTTP_RAW_PATH}
#update_counters 0

#./src/test/bash/response_resolver_test.sh
#update_counters 0


./../solve/src/test/string_buffer_test
update_counters 0
#valgrind --error-exitcode=15 --leak-check=full --show-leak-kinds=all --errors-for-leak-kinds=all --run-cxx-freeres=yes ./../solve/src/test/string_buffer_test

#./../solve/src/test/tcp_client_test
#update_counters 0

#./../solve/src/test/buffered_reader_test
#update_counters 0

./../solve/src/test/file_reader_test
update_counters 0
#valgrind --error-exitcode=15 --leak-check=full --show-leak-kinds=all --errors-for-leak-kinds=all --run-cxx-freeres=yes ./../solve/src/test/file_reader_test

./../solve/src/test/address_resolver_test
update_counters 0

#./src/test/bash/request_resolver_test.sh
#update_counters 0

#valgrind --error-exitcode=15 --leak-check=full --show-leak-kinds=all --errors-for-leak-kinds=all --run-cxx-freeres=yes ./../solve/testhttp_raw acrocephalus.vaibla.net:80 ciasteczka.txt http://acrocephalus.vaibla.net/


echo -e "\n=================================="
echo -e "Passed all: ${PASSED_TESTS} / ${ALL_TESTS}"
echo -e "==================================\n"

rm -f *.o