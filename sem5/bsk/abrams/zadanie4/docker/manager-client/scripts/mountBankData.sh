#!/usr/bin/env bash

mkdir ~/bank
sshfs "$1"@server:/home/bank ~/bank -o IdentityFile=/home/worker1/.ssh/id_rsa
