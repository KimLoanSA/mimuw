CC = gcc
CFLAGS = -Wall -Wextra -std=c17 -O2
LDFLAGS =

.PHONY: all clean

all: response_resolver_test.o request_resolver_test.o

# tutaj pododawac .h plikow ktorych uzywacie
response_resolver_test.o: c/response_resolver_test.c ../../../docker-test/src/test/c/interfaces/response_resolver_test.h
	$(CC) $(CFLAGS) -c $<

request_resolver_test.o: c/request_resolver_test.c ../../../docker-test/src/test/c/interfaces/request_resolver_test.h
	$(CC) $(CFLAGS) -c $<

clean:
	rm -r -f *.o