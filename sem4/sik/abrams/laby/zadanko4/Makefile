TARGET: nk-server-udp nk-client-udp

CC	= gcc
CFLAGS	= -Wall -O2
LFLAGS	= -Wall

nk-server-udp.o nk-client-udp.o err.o: err.h

nk-server-udp: nk-server-udp.o err.o
	$(CC) $(LFLAGS) $^ -o $@

nk-client-udp: nk-client-udp.o err.o
	$(CC) $(LFLAGS) $^ -o $@

.PHONY: clean TARGET
clean:
	rm -f nk-server-udp nk-client-udp *.o *~ *.bak
