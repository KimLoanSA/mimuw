#!/usr/bin/env bash

function validateExitcode() {
  if [[ $1 == 1 ]]
  then
    echo -e "passed"
  else
    echo -e "invalid exit code\n"
    exit 1
  fi
}

echo -e "-- defaultRadioProxyArgumentsResolverInvalidTest tests --"

./defaultRadioProxyArgumentsResolverMetadataInvalid 2> /dev/null
validateExitcode $?

./defaultRadioProxyArgumentsResolverNoHostTest 2> /dev/null
validateExitcode $?

./defaultRadioProxyArgumentsResolverNoPortTest 2> /dev/null
validateExitcode $?

./defaultRadioProxyArgumentsResolverNoResourceTest 2> /dev/null
validateExitcode $?

./defaultRadioProxyArgumentsResolverTimeout0Test 2> /dev/null
validateExitcode $?


echo -e "-- all tests passed --\n"