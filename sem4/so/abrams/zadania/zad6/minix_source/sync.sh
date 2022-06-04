#!/bin/bash

cd "$(dirname "$0")" # set current working directory to the directory of the script

rsync -uav --exclude=".git" ./usr/include/ minix:/usr/include/
rsync -uav --exclude=".git" ./usr/src/ minix:/usr/src/
scp ioc_example.c minix:~/ioc_example.c
scp system.conf minix:/etc/system.conf
scp test.py minix:~/test.py