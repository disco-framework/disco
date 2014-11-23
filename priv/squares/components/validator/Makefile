BINARY = dist/build/validator/validator
PROF_OPTS = -prof -auto-all -caf-all -fforce-recomp

.PHONY: all compile configure run clean hlint init-sandbox

all: compile

.cabal.sandbox:
	cabal sandbox init

prepare: .cabal.sandbox
	cabal install --only-dependencies
	cabal configure

compile: prepare
	cabal build

prof-compile: prepare
	cabal build --ghc-options='-o $(BINARY)-prof $(PROF_OPTS)'

run:
	@./$(BINARY)

clean:
	cabal clean
	rm -f $(BINARY).prof $(BINARY)-prof

hlint:
	hlint *.hs

perftest: prof-compile
	time for i in `seq 1 1000`; do cat test_prop; done | ./$(BINARY)-prof +RTS -p
