#!/bin/bash

BASE_IMAGE_VERSION="2_0"
BASE_IMAGE_NAME="mimuw.sik.abrams/pld-linux-libs"

IMAGE_VERSION="3_0"
IMAGE_NAME="mimuw.sik.abrams/testhttp-tests"

echo -e "SPRAWDZANIE CZY TRZEBA BUDOWAC OBRAZ BAZOWY\n\n"
if [[ "$(docker images -q ${BASE_IMAGE_NAME}:${BASE_IMAGE_VERSION} 2> /dev/null)" == "" ]]; then
  echo -e "BUDOWANIE BAZOWEGO OBRAZU\n\n"
  (cd docker-test/build; ./buildBaseDockerImage.sh)
fi

echo -e "BUDOWANIE OBRAZU\n\n"
docker build -t "${IMAGE_NAME}:${IMAGE_VERSION}" .

echo -e "URUCHAMIANIE OBRAZU I TESTOW\n\n"
docker run -it ${IMAGE_NAME}:${IMAGE_VERSION}

echo -e "CZYSZCZENIE OBRAZOW\n\n"
docker rmi -f $(docker images -qa -f 'dangling=true')