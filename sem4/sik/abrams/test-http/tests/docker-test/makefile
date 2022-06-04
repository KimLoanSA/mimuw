CC = gcc
CFLAGS = -Wall -Wextra -std=c17 -O2
LDFLAGS =

.PHONY: all clean

all: response_resolver_test request_resolver_test

# tutaj pododawac .o plikow ktorych uzywacie
response_resolver_test: response_resolver_test_impl.o ../solve/src/test/response_resolver_test.o
	$(CC) $(LDFLAGS) -o $@ $^

response_resolver_test_impl.o: src/test/c/response_resolver_test_impl.c src/test/c/interfaces/response_resolver_test.h
	$(CC) $(CFLAGS) -c $<

request_resolver_test: request_resolver_test_impl.o ../solve/src/test/request_resolver_test.o
	$(CC) $(LDFLAGS) -o $@ $^

request_resolver_test_impl.o: src/test/c/request_resolver_test_impl.c src/test/c/interfaces/request_resolver_test.h
	$(CC) $(CFLAGS) -c $<