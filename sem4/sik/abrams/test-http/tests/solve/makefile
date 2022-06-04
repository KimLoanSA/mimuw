CC = gcc
CFLAGS = -Wall -Wextra -std=c17 -O2
LDFLAGS =

.PHONY: all clean

all: testhttp_raw

# tutaj pewnie trzeba duzo dodac zeby sie budowalo, ale tylko swoje moduly
testhttp_raw: testhttp_raw.o
	$(CC) $(LDFLAGS) -o $@ $^

testhttp_raw.o: src/main/c/testhttp_raw.c
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o testhttp_raw