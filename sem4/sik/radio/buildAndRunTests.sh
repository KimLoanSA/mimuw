#!/usr/bin/env bash

echo -e "building tests ..."

make clean
make

echo -e "running tests ..."

./programArgumentsParserTest
./defaultRadioProxyArgumentsResolverTest
./src/test/bash/defaultRadioProxyArgumentsResolverInvalidTest.sh
./responseResolverTest
./udpProxyArgumentsResolverTest
./udpClientsStorageTest
#./radioClientCommunicationParserTest