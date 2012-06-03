.PHONY: all check

all:

check:
	for f in test/*.scm ; do gosh -I. $$f ; done
