#!/bin/bash

target_dir=../target

# budowanie rozwiazania
nasm -f elf64 -o $2/dcl.o $1/dcl.asm
ld -o $2/dcl $2/dcl.o

# budowanie testow
cp $3/gen.py $2/gen.py
gcc $4/ref.c -o $2/ref
