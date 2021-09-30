#!/bin/bash

target_dir=../target

nasm -f elf64 -w+all -w+error -o $2/dcl.o $1/dcl.asm
ld --fatal-warnings -o $2/dcl $2/dcl.o

gcc $1/ref.c -o $2/ref

cp $1/gen.py $2/gen.py
