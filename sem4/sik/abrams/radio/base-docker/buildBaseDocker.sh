#!/usr/bin/env bash

BASE_IMAGE_VERSION="1_0"
BASE_IMAGE_NAME="mimuw.sik.abrams/ubuntu-libs"

docker build -t "${BASE_IMAGE_NAME}:${BASE_IMAGE_VERSION}" .
