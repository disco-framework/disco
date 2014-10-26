BINARY = changer
GHC_OPTS = -Wall -O2

.PHONY: all compile run clean hlint

all: compile

compile:
	ghc --make Main -o $(BINARY) $(GHC_OPTS)

run:
	@./$(BINARY)

clean:
	rm -f *.o *.hi $(BINARY)

hlint:
	hlint *.hs
