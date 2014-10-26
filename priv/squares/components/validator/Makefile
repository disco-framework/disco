BINARY = validator
GHC_OPTS = -Wall -O2
PROF_OPTS = -prof -auto-all -caf-all -fforce-recomp

.PHONY: all compile run clean hlint

all: compile

compile:
	ghc --make Main -o $(BINARY) $(GHC_OPTS)

prof-compile:
	ghc --make Main -o $(BINARY)-prof $(GHC_OPTS) $(PROF_OPTS)

run:
	@./$(BINARY)

clean:
	rm -f *.o *.hi $(BINARY) $(BINARY).prof $(BINARY)-prof

hlint:
	hlint *.hs

perftest: prof-compile
	time for i in `seq 1 1000`; do cat test_prop; done | ./$(BINARY)-prof +RTS -p
