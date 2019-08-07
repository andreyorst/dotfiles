PROGRAM = $(shell basename $(CURDIR))

CC      = clang
CFLAGS  = $(shell cat compile_flags.txt | tr '\n' ' ')
LDFLAGS =

SOURCES = $(wildcard *.c)
OBJECTS = $(SOURCES:.c=.o)

all: $(PROGRAM)

${PROGRAM}: $(OBJECTS)
	$(CC) -o $@ $^ $(LDFLAGS)

%.o: %.c
	$(CC) -o $@ -c $< $(CFLAGS)
	@mkdir -p ./bin
	$(CC) -o ./bin/$(PROGRAM) *.o

.PHONY: $(PROGRAM) clean run depend help

clean:
	@rm -rf ./bin *.o .depend

run: main.o
	@./bin/$(PROGRAM)

.depend:
	$(CC) -MM *.c > .depend

help:
	@echo "make      -- build executable"
	@echo "make run  -- run executable. Build if 'bin/$(PROGRAM)' not exist"

-include .depend
