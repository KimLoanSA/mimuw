#!/usr/bin/env bash

docker image rm -f bsk
docker build -t bsk .
docker run --cap-add=SYS_PTRACE --security-opt seccomp=unconfined -it bsk
