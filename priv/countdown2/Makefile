SHELL=/bin/bash

DIRS := components/validator \
        workers/pwb_72       \
        workers/pwb_73       \
        workers/pwb_74

all clean:
	@summary="";                                                     \
	for e in $(DIRS); do                                             \
	    echo "******************************";                       \
	    echo "** make $@ -sC $$e";                                   \
	    echo "******************************";                       \
	    if make $@ -sC $$e;                                          \
	    then echo "==> OK";                                          \
	         summary="$$summary$$(printf '%-10s ==> OK'     $$e)\n"; \
	    else echo "==> FAILED !";                                    \
	         summary="$$summary$$(printf '%-10s ==> FAILED' $$e)\n"; \
	    fi; echo;                                                    \
	done;                                                            \
	echo -e "\n$$summary"
