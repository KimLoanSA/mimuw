#!/bin/bash

qemu-img create \
    -f qcow2 \
    -o backing_file=~/Projects/so/minix.img minix-machine.img
