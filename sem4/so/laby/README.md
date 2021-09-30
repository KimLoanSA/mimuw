### `lab2i3/a4`
```
nasm -f elf64 -o mac.o mac.asm
gcc -c -Wall -O2 -o mac_test.o mac_test.c
gcc -o mac_test mac.o mac_test.o
./mac_test
```

### `lab2i3/a6`
```
nasm -f elf64 -o hello_world.o hello_world.asm
ld -o hello_world hello_world.o
./hello_world
```

### `lab2i3/a8`
```
gcc mac.c -o mac
./mac
```

### `lab4`
```
make
./inc_thread_test_naive 3 10000000
```
