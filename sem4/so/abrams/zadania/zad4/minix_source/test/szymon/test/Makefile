all: halving halving_even levels nochild nograndchild nograndparent noparent noself nosystem
LDFLAGS	= -Wall -Wextra

%: %.c
	$(CC) -o $@ $^ $(LDFLAGS)

all: $(tests)

.PHONY: clean
clean:
	rm -f *.o halving halving_even levels nochild nograndchild nograndparent noparent noself nosystem
