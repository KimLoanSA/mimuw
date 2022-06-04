PROGRAMS = inc_thread_test_naive
CC = gcc
CFLAGS = -Wall -O2

all: $(PROGRAMS)

err.o: err.c err.h

inc_thread_naive.o: inc_thread_naive.asm
	nasm -f elf64 -o $@ $<

inc_thread_test.o: inc_thread_test.c inc_thread.h err.h
	$(CC) -c $(CFLAGS) -o $@ $<

inc_thread_test_naive: inc_thread_test.o inc_thread_naive.o err.o
	$(CC) -lpthread -o $@ $^

.PHONY: all clean

clean:
	rm -rf $(PROGRAMS) *.o
