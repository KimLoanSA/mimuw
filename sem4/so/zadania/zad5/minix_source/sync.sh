#!/bin/bash

cd "$(dirname "$0")" # set current working directory to the directory of the script

rsync -uav --exclude=".git" ./usr/include/ minix:/usr/include/
rsync -uav --exclude=".git" ./usr/src/ minix:/usr/src/

scp build.sh minix:~/build.sh
scp test.c  minix:~/test.c