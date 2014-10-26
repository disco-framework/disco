BINARY = solver
GHC_OPTS = -Wall -O2
PROF_OPTS = -prof -auto-all -caf-all -fforce-recomp

.PHONY: all compile run clean hlint

all: compile

name:
	@echo "Hobbits-to-Isengart-Taker"

compile:
	ghc --make Main -o $(BINARY) $(GHC_OPTS)

prof-compile:
	ghc --make Main -o $(BINARY)-prof $(GHC_OPTS) $(PROF_OPTS)

run:
	@./$(BINARY)

clean:
	rm -f *.o *.hi $(BINARY) $(BINARY)-prof

hlint:
	hlint *.hs

perftest: compile
	time cat test_prob | ./$(BINARY)

prof: prof-compile
	cat test_prob | ./$(BINARY)-prof +RTS -p
