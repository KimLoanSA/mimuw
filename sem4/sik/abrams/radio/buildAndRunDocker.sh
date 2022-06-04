#!/usr/bin/env bash

IMAGE_VERSION="1_0"
IMAGE_NAME="mimuw.sik.abrams/radio"

docker build -t "${IMAGE_NAME}:${IMAGE_VERSION}" .
docker run -it ${IMAGE_NAME}:${IMAGE_VERSION}

docker rmi -f $(docker images -qa -f 'dangling=true')