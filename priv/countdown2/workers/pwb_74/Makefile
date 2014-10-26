PROG = ./pwb2007.tcl

.PHONY: all name run

all:
	@echo "nothing to do..."

run:
	@$(PROG)

name:
	@echo "Jachthund"

test:
	@(echo '[1,1,2,3,5,8]'; echo '42') | $(PROG)

test2:
	@(echo '[2,4,40]'; echo '42') | $(PROG)

test3:
	@(echo '[1,1,1,1,2,2,2,2,10,10,10,500,1000]'; echo '1') | $(PROG)

clean:
