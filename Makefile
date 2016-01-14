source = $(wildcard *.hs)
binaries = $(basename $(source))

all: $(binaries)

clean: tidy
	rm -f $(binaries)
	rm -f *.hi
	rm -f *.o

tidy:
	rm -f *~

%: %.hs
	cabal exec ghc -- -O2 -W$(W) $<