

## contest specific configuration file
CONFIG_FILE            = countdown2.config

## log level
## possible values: debug, info, notice, warning, error, critical, alert, emergency
LOG_LEVEL              = info


## distribution configuration

    ## list of hosts to distribute the compiled erlang code
    REMOTE_IPS         = $(shell scripts/get_ips_from_config $(CONFIG_FILE) | sort --uniq)

    ## remote working directory ('remote_node_path_prefix' from config)
    REMOTE_PATH_PREFIX = $(shell scripts/get_path_prefix_from_config $(CONFIG_FILE))


## erlang configuration

    ## The local host name or ip address for the erlang node communication.
    ## Use 127.0.0.1 here and for the workers (in the configuration file)
    ## to start all erlang nodes locally and avoid using ssh.
    LOCAL_IP           = 127.0.0.1
    LOCAL_IP          ?= $(shell scripts/get_local_ip)

    ## name of the main erlang node
    NODE_NAME          = disco

    ## erlang bytecode location
    BEAM_DIRS          = _build/default/lib/*/ebin/

    ## erlang 'magic cookie' for node authentication
    ERL_COOKIE         = disco_framework_erl_secret

## erlang build tool
REBAR                  = ./rebar3

## code coverage, should only be enabled for integration test
COVER_ENABLED          = false

## set log level parameter for non default log levels
ifneq ($(LOG_LEVEL), info)
    ERL_DEBUG_ARGS     = -lager     handlers '[ {lager_console_backend, $(LOG_LEVEL)},           \
                                                {lager_file_backend, [ {"log/debug.log", debug}, \
                                                                       {"log/info.log", info},   \
                                                                       {"log/error.log", error}  \
                                                                     ] }                         \
                                              ]'
endif

PRINT_CAPTION          = @echo ""; echo "===== $@ ====="

.PHONY: all compile distribute run doc clean

all: run

compile:
	$(PRINT_CAPTION)
	$(REBAR) $@

distribute: compile
	$(PRINT_CAPTION)
	@for ip in $(REMOTE_IPS); do \
	    if ping $$ip -q -c1 -W1 >/dev/null; then \
	        echo "==> copy code to $$ip"; \
	        rsync -rERl $(BEAM_DIRS) $$ip:$(REMOTE_PATH_PREFIX); \
	    fi \
	done

run: compile
	$(PRINT_CAPTION)
	erl -name	   $(NODE_NAME)@$(LOCAL_IP) \
	    -setcookie	   $(ERL_COOKIE) \
	    -rsh           ssh \
	    -pa		   $(BEAM_DIRS) \
	    -config        $(CONFIG_FILE) \
	    -sasl          sasl_error_logger false \
	    -cover_enabled $(COVER_ENABLED) \
	    -s             disco_app \
	    $(ERL_DEBUG_ARGS)

doc: compile
	$(PRINT_CAPTION)
	cp doc/edoc.css apps/worker/doc/edoc.css
	$(REBAR) $@ skip_deps=true
	@echo "--"
	@echo "generated code docs."
	@echo "Entry point at doc/edoc/index.html"

clean:
	$(PRINT_CAPTION)
	$(REBAR) $@

clean_all: clean
	$(PRINT_CAPTION)
	rm -rf log/
	rm -rf autosaves/

#
# TESTS
#

.PHONY: test_all

test_all: dialyze eunit_test integration_test
	@echo "-- all tests successful! --"

#
# eunit
#

.PHONY: eunit_test

eunit_test: compile
	$(PRINT_CAPTION)
	$(REBAR) eunit skip_deps=true
#	xdg-open eunit.html &

eunit_test_%: compile ./src/test/%_tests.erl
	$(PRINT_CAPTION)
	$(REBAR) eunit skip_deps=true suites=$*
	xdg-open .eunit/index.html &

#
# integration test
#

.PHONY: integration_test

integration_test:
	$(MAKE) -s CONFIG_FILE=integration-test.config COVER_ENABLED=true run
	@echo "-- INTEGRATION TEST RESULTS --"
	@cat log/integration-test.log
	@tail -n1 log/integration-test.log | grep OK

#
# dialyzer
#

FILTER_WARNINGS     = fgrep -v -f ./dialyzer.ignore-warnings

.PHONY: dialyze

dialyze:
	$(PRINT_CAPTION)
	$(REBAR) dialyzer | $(FILTER_WARNINGS)
