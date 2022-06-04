#!/bin/bash

echo -e "testy abramsa\n"

cd testhttp/docker-test || exit
./build/buildSolve.sh
./build/buildTests.sh
./runTests.sh

echo -e "dzieki za testy abramsa\n"