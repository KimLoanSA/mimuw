CC = gcc
CFLAGS = -Wall -Wextra -O2
LDFLAGS =

.PHONY: all clean

all: testhttp_raw

testhttp_raw: testhttp_raw.o program_arguments_validator.o response_resolver.o logger.o safe_memory.o string_buffer.o tcp_client.o buffered_reader.o file_reader.o address_resolver.o request_sender.o
	$(CC) $(LDFLAGS) -o $@ $^

logger.o: src/main/c/logger.c src/main/c/logger.h
	$(CC) $(CFLAGS) -c $<

safe_memory.o: src/main/c/safe_memory.c src/main/c/safe_memory.h src/main/c/logger.h
	$(CC) $(CFLAGS) -c $<

string_buffer.o: src/main/c/string_buffer.c src/main/c/string_buffer.h src/main/c/safe_memory.h
	$(CC) $(CFLAGS) -c $<

file_reader.o: src/main/c/file_reader.c src/main/c/file_reader.h src/main/c/safe_memory.h src/main/c/logger.h
	$(CC) $(CFLAGS) -c $<

tcp_client.o: src/main/c/tcp_client.c src/main/c/tcp_client.h src/main/c/safe_memory.h src/main/c/logger.h
	$(CC) $(CFLAGS) -c $<

address_resolver.o: src/main/c/address_resolver.c src/main/c/address_resolver.h
	$(CC) $(CFLAGS) -c $<

buffered_reader.o: src/main/c/buffered_reader.c src/main/c/buffered_reader.h src/main/c/safe_memory.h src/main/c/string_buffer.h src/main/c/tcp_client.h
	$(CC) $(CFLAGS) -c $<

program_arguments_validator.o: src/main/c/program_arguments_validator.c src/main/c/program_arguments_validator.h src/main/c/logger.h
	$(CC) $(CFLAGS) -c $<

response_resolver.o: src/main/c/response_resolver.c src/main/c/response_resolver.h src/main/c/string_buffer.h src/main/c/buffered_reader.h
	$(CC) $(CFLAGS) -c $<

testhttp_raw.o: src/main/c/testhttp_raw.c src/main/c/program_arguments_validator.h
	$(CC) $(CFLAGS) -c $<

request_sender.o: src/main/c/request_sender.c src/main/c/request_sender.h src/main/c/safe_memory.h src/main/c/tcp_client.h src/main/c/file_reader.h
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o testhttp_raw