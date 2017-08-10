CC      = ghc

SOURCES = $(shell find . -name '*.hs')


.SUFFIXES: .hs .o

.PHONY: all clean

all: ../Main

../Main: $(SOURCES)
	$(CC) Main.hs -o ../Main
	rm *.o *.hi


clean:
	rm -rf $(objdir)
	rm -f emulate
	rm -f $(wildcard *.bin)
