.PHONY: all check

all:

check:
	gosh -I. test/*.scm
