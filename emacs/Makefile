EMACS ?= emacs

all: generate

generate:
	${EMACS} -Q --script "init.el" test/test.org
clean:
	$(RM) *.html

.PHONY: all generate
