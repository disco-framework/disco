
SRC       = brutus.erl
BEAM      = $(SRC:.erl=.beam)
ERLC_ARGS = +native
ifdef MODULO
  ERLC_ARGS += -Dmodulo
endif
ERL_ARGS  = -noshell -run brutus

.PHONY: all name run debug clean

%.beam: %.erl
	erlc $(ERLC_ARGS) "$<"

all: $(BEAM)

name:
	@echo "Brutus"

run: $(BEAM)
	@erl $(ERL_ARGS) 2>/dev/null

debug: $(BEAM)
	@erl $(ERL_ARGS) >/dev/null

clean:
	rm -f $(BEAM)


test1: $(BEAM) ; @(echo [1,1,2,3,5,8];                                echo 42) | $(MAKE) -s debug
test2: $(BEAM) ; @(echo [1,1,1,1,2,2,2,2,10,10,10,500,1000];          echo  1) | $(MAKE) -s debug
test3: $(BEAM) ; @(echo [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610]; echo 42) | $(MAKE) -s debug
test4: $(BEAM) ; @(echo [1,1,2,3,5,8,13,21];                          echo 34) | $(MAKE) -s debug
