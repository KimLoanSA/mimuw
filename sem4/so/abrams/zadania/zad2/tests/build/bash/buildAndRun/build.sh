#!/bin/bash

# $1 - src/assembly, $2 - src/c, $3 - target

nasm -f elf64 -w+all -w+error -o $3/pix.o $1/pix.asm
gcc -std=c11 -Wall -Wextra -O2  -lpthread -o $3/pix $2/*.c $3/pix.o
