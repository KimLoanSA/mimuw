#!/bin/bash

IMAGE_NAME_WITH_VERSION=sik:1_0

docker build -t "${IMAGE_NAME_WITH_VERSION}" .
docker run -it ${IMAGE_NAME_WITH_VERSION}
docker rmi -f $(docker images -qa -f 'dangling=true')