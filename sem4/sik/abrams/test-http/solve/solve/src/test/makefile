CC = gcc
CFLAGS = -Wall -Wextra -std=c17 -O2
LDFLAGS =

.PHONY: all clean

all: string_buffer_test tcp_client_test buffered_reader_test file_reader_test response_resolver_test.o address_resolver_test request_resolver_test.o

response_resolver_test.o: c/response_resolver_test.c ../../../docker-test/src/test/c/interfaces/response_resolver_test.h ../main/c/response_resolver.h
	$(CC) $(CFLAGS) -c $<

string_buffer_test: string_buffer_test.o ../../string_buffer.o  ../../safe_memory.o  ../../logger.o
	$(CC) $(LDFLAGS) -o $@ $^

string_buffer_test.o: c/string_buffer_test.c ../main/c/string_buffer.h
	$(CC) $(CFLAGS) -c $<

file_reader_test: file_reader_test.o ../../file_reader.o  ../../safe_memory.o  ../../logger.o
	$(CC) $(LDFLAGS) -o $@ $^

file_reader_test.o: c/file_reader_test.c ../main/c/file_reader.h
	$(CC) $(CFLAGS) -c $<

tcp_client_test: tcp_client_test.o  ../../safe_memory.o  ../../logger.o ../../tcp_client.o
	$(CC) $(LDFLAGS) -o $@ $^

tcp_client_test.o: c/tcp_client_test.c ../main/c/tcp_client.h
	$(CC) $(CFLAGS) -c $<

buffered_reader_test: buffered_reader_test.o  ../../safe_memory.o  ../../logger.o ../../tcp_client.o ../../buffered_reader.o ../../string_buffer.o
	$(CC) $(LDFLAGS) -o $@ $^

buffered_reader_test.o: c/buffered_reader_test.c ../main/c/buffered_reader.h ../main/c/tcp_client.h
	$(CC) $(CFLAGS) -c $<

address_resolver_test: address_resolver_test.o  ../../address_resolver.o ../../safe_memory.o ../../logger.o
	$(CC) $(LDFLAGS) -o $@ $^

address_resolver_test.o: c/address_resolver_test.c ../main/c/address_resolver.h ../main/c/safe_memory.h
	$(CC) $(CFLAGS) -c $<

request_resolver_test.o: c/request_resolver_test.c ../../../docker-test/src/test/c/interfaces/request_resolver_test.h ../main/c/request_sender.h ../main/c/address_resolver.h
	$(CC) $(CFLAGS) -c $<

clean:
	rm -r -f *.o string_buffer_test