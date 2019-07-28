PROGRAM=$(shell basename $(CURDIR))
CFLAGS=$(shell cat compile_flags.txt | tr '\n' ' ')

all: $(PROGRAM).o

$(PROGRAM).o:
	@clang -c $(CFLAGS) *.c
	@mkdir -p ./bin
	@clang -o ./bin/$(PROGRAM) *.o

.PHONY: clean run depend help

clean:
	@rm -rf ./bin *.o .depend

run:: $(PROGRAM).o
	@./bin/$(PROGRAM)

.depend:
	@clang -MM *.c > .depend

help:
	@echo "make      -- build executable"
	@echo "make run  -- run executable. Build if 'bin/$(PROGRAM)' not exist"

-include .depend
