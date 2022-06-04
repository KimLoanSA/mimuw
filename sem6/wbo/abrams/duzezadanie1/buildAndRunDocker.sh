#!/usr/bin/env bash

docker image rm -f wbo-bigtask-1
docker build -t wbo-bigtask-1 .
docker run -it wbo-bigtask-1