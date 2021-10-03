EXECS = myping spoofping

CPPFLAGS = -Wall
LDFLAGS	= 
LDLIBS = 

TARGET: $(EXECS)

myping.o err.o dropnobody.o spoofping.o: err.h

myping: myping.o err.o in_cksum.o dropnobody.o

spoofping: spoofping.o err.o in_cksum.o dropnobody.o

.PHONY: clean TARGET
clean:
	rm -f $(EXECS) *.o *~ 
