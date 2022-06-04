#!/usr/bin/env bash

docker image rm debian-bsk-4
docker build -t debian-bsk-4 .
