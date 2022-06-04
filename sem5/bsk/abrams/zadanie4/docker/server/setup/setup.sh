#!/usr/bin/env bash

service ssh start
service ssh status

mkdir /home/bank
echo "BSK 4" >> /home/bank/data
