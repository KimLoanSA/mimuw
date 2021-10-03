TARGET: nk-server-tcp nk-client-tcp

CC	= cc
CFLAGS	= -Wall -O2
LFLAGS	= -Wall

nk-server-tcp.o nk-client-tcp.o err.o: err.h

nk-server-tcp: nk-server-tcp.o err.o
	$(CC) $(LFLAGS) $^ -o $@

nk-client-tcp: nk-client-tcp.o err.o
	$(CC) $(LFLAGS) $^ -o $@


.PHONY: clean TARGET
clean:
	rm -f nk-server-tcp nk-client-tcp nums *.o *~ *.bak
