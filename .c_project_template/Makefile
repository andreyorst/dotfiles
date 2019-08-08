PROGRAM = ./bin/$(shell basename $(CURDIR))

CC      = clang
CFLAGS  = $(shell cat compile_flags.txt | tr '\n' ' ')
LDFLAGS = $(CFLAGS)

SOURCES = $(wildcard *.c)
OBJECTS = $(SOURCES:.c=.o)

all: $(PROGRAM)

${PROGRAM}: $(OBJECTS)
	@mkdir -p ./bin
	$(CC) -o $@ $^ $(LDFLAGS)

%.o: %.c
	$(CC) -o $@ -c $< $(CFLAGS)

.PHONY: clean run depend help

clean:
	@rm -rf ./bin *.o .depend

run: $(PROGRAM)
	@$(PROGRAM)

.depend:
	$(CC) -MM *.c > .depend

help:
	@echo "make      -- build executable"
	@echo "make run  -- run executable. Build if 'bin/$(PROGRAM)' not exist"

-include .depend
