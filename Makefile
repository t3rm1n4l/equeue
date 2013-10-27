SHELL=/bin/sh

EFLAGS=-pa ebin

.PHONY: ebins

all: ebins

ebins:
	test -d ebin || mkdir ebin
	erl $(EFLAGS) -make

test:
	erl $(EFLAGS) -s queue_tests run_tests -s init stop

clean:
	rm -rf ebin
