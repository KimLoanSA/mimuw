#!/bin/bash

cd "$(dirname "$0")" # set current working directory to the directory of the script

#rsync -uav --exclude=".git" ./usr/include/ minix:/usr/include/
#rsync -uav --exclude=".git" ./usr/src/ minix:/usr/src/

scp -r test/change-parent-test.c minix:~/test/change-parent-test.c
scp -r test/original-parent-test.c minix:~/test/original-parent-test.c
scp -r test/buildAndRunTests.sh minix:~/test/buildAndRunTests.sh
scp -r test/ciolek/ minix:~/test/ciolek
scp -r test/zewie/ minix:~/test/zewie
scp -r test/szymon/ minix:~/test/szymon