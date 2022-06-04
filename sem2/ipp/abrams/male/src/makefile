CC = gcc
CFLAGS = -Wall -Wextra -std=c11 -O2
LDFLAGS =

.PHONY: all clean

all: quantization

quantization: main.o iolib.o listlib.o trielib.o dsulib.o
	$(CC) $(LDFLAGS) -o $@ $^

iolib.o: iolib.c iolib.h
	$(CC) $(CFLAGS) -c $<

listlib.o: listlib.c listlib.h
	$(CC) $(CFLAGS) -c $<

dsulib.o: dsulib.c dsulib.h
	$(CC) $(CFLAGS) -c $<

trielib.o: trielib.c trielib.h
	$(CC) $(CFLAGS) -c $<

main.o: main.c iolib.h listlib.h dsulib.h trielib.h
	$(CC) $(CFLAGS) -c $<

clean:
	-rm -fr iolib.o
	-rm -fr trielib.o
	-rm -fr listlib.o
	-rm -fr dsulib.o
	-rm -fr main.o
	-rm -fr quantization
	
	
