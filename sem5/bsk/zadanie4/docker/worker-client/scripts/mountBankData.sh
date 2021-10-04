#!/usr/bin/env bash

mkdir ~/bank
sshfs "$1"@server:/home/bank ~/bank
