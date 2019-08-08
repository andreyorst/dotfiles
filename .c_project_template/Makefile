PROGRAM = $(shell basename $(CURDIR))

CC      = clang
CFLAGS  = $(shell cat compile_flags.txt | tr '\n' ' ')
LDFLAGS = $(CFLAGS)

SOURCES = $(wildcard *.c)
OBJECTS = $(SOURCES:.c=.o)

all: ./bin/$(PROGRAM)

./bin/${PROGRAM}: $(OBJECTS)
	@mkdir -p ./bin
	$(CC) -o $@ $^ $(LDFLAGS)

%.o: %.c
	$(CC) -o $@ -c $< $(CFLAGS)

.PHONY: $(PROGRAM) clean run depend help

clean:
	@rm -rf ./bin *.o .depend

run: ./bin/$(PROGRAM)
	@./bin/$(PROGRAM)

.depend:
	$(CC) -MM *.c > .depend

help:
	@echo "make      -- build executable"
	@echo "make run  -- run executable. Build if 'bin/$(PROGRAM)' not exist"

-include .depend
