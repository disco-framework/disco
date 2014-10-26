
REBAR = ./rebar

.PHONY: all depends compile compile_erl compile_dart run clean test

all: clean compile

depends:
	$(REBAR) get-deps
	$(REBAR) update-deps

compile:
	@echo '##### GET DEPS #####'
	$(REBAR) get-deps
	@echo '##### COMPILE #####'
ifdef MODULO
	$(REBAR) compile -Dmodulo
else
	$(REBAR) compile
endif

run:
	@erl -pa        ebin/ deps/*/ebin/ \
	     -s         pw \
	     -noshell

shell:
	@erl -pa        ebin/ deps/*/ebin/ \
	     -s         pw

clean:
	$(REBAR) clean

test: compile
	@(echo "[1,1,2,3,5,8]"; echo 42; cat) | \
	 erl -pa        ebin/ deps/*/ebin/ \
	     -s         pw \
	     -noshell

simpletest: compile
	@(echo "[1,1,2,3,5]"; echo 15; cat) | \
	 erl -pa        ebin/ deps/*/ebin/ \
	     -s         pw \
	     -noshell

sametest: compile
	@(echo "[2,2,2,2,2,2,2,2,2,3]"; echo 42; cat) | \
	 erl -pa        ebin/ deps/*/ebin/ \
	     -s         pw \
	     -noshell

tim_test: compile
	@(echo "[2, 3, 5, 7, 11, 13]"; echo 17; cat) | \
	 erl -pa        ebin/ deps/*/ebin/ \
	     -s         pw \
	     -noshell

tim_test2: compile
	@(echo "[2, 3, 5, 7, 11, 13, 17, 19, 23, 29]"; echo 31; cat) | \
	 erl -pa        ebin/ deps/*/ebin/ \
	     -s         pw \
	     -noshell

bigtest: compile
	@(echo "[1,1,2,3,5,8,13,21,34,55,89,144,144,144,144,144]"; echo 100; cat) | \
	 erl -pa        ebin/ deps/*/ebin/ \
	     -s         pw \
	     -noshell
name:
	@echo 'number dumper'


#
# dialyzer
#

PLT = plt
PLT_LIBS = $(wildcard deps/*/ebin)
DIALYZER_APPS_PATHS = ebin
DIALYZER_WARN_OPTS = -Werror_handling -Wunmatched_returns

check_plt:
	dialyzer --check_plt --plt $(PLT) $(PLT_LIBS)

build_plt: $(PLT)

$(PLT):
	dialyzer --build_plt --output_plt $(PLT) $(PLT_LIBS) --apps erts kernel stdlib

dialyze: compile $(PLT)
	@echo '##### DIALYZE #####'
	dialyzer -nn $(DIALYZER_WARN_OPTS) --fullpath --plt $(PLT) $(DIALYZER_APPS_PATHS) | \
	    fgrep -v -f ./dialyzer.ignore-warnings

wunder-dialyze: compile build_plt
	@echo '##### WUNDER-DIALYZE #####'
	dialyzer -nn $(DIALYZER_WARN_OPTS) -Wunderspecs --fullpath --plt $(PLT) $(DIALYZER_APPS_PATHS) | \
	    fgrep -v -f ./dialyzer.ignore-warnings

clean_plt:
	rm $(PLT)

