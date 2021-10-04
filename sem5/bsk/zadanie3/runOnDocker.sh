#!/usr/bin/env bash

docker image rm -f bsk-3
docker build -t bsk-3 .
docker run -it bsk-3