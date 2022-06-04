#!/usr/bin/env bash

docker image rm debian-bsk-6 -f
docker build -t debian-bsk-6 .
