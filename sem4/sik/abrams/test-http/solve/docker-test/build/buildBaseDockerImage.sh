#!/bin/bash

BASE_IMAGE_VERSION="1_0"
BASE_IMAGE_NAME="mimuw.sik.abrams/pld-linux-libs"

cd ..
docker build -t "${BASE_IMAGE_NAME}:${BASE_IMAGE_VERSION}" .