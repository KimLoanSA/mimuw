CC = gcc
CFLAGS = -Wall
TARGETS = poll-server-telnet

all: $(TARGETS) 

err.o: err.c err.h

poll_server.o: poll-server-telnet.c err.h

poll-server-telnet.o: poll-server-telnet.c err.h

poll-server-telnet: poll-server-telnet.o err.o

clean:
	rm -f *.o $(TARGETS) 
