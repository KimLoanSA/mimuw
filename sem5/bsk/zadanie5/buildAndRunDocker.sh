#!/usr/bin/env bash

docker image rm bsk-5 -f
docker build -t bsk-5 .
docker run -p 443:443 bsk-5
