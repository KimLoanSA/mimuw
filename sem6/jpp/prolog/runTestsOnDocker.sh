#!/usr/bin/env bash

docker image rm "jpp-verify" -f
docker build -t "jpp-verify" .
docker run -it "jpp-verify"
