PROGRAM = ./bin/$(shell basename $(CURDIR))

CC      = clang
CFLAGS  = $(shell cat compile_flags.txt | tr '\n' ' ')
LDFLAGS = $(CFLAGS)

SOURCES = $(shell find -type f -name '*.c')
OBJECTS = $(SOURCES:.c=.o)

all: $(PROGRAM)

.PHONY: all clean run depend help

${PROGRAM}: $(OBJECTS)
	@mkdir -p ./bin
	$(CC) -o $@ $^ $(LDFLAGS)

%.o: %.c
	$(CC) -o $@ -c $< $(CFLAGS)

clean:
	@rm -rf ./bin *.o .depend

run: $(PROGRAM)
	@$(PROGRAM)

.depend:
	$(CC) -MM $(CFLAGS) *.c > .depend

help:
	@echo "make       -- build executable"
	@echo "make run   -- run executable. Build if 'bin/$(PROGRAM)' not exist"
	@echo "make clean -- clean project from build files and delete executable"

-include .depend
