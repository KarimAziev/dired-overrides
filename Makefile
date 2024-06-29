EMACS ?= emacs

ELS = dired-overrides.el
ELCS = dired-overrides.elc
TEST_ELS = tests/test-bootstrap.el tests/dired-overrides-test.el


ifeq (test, $(firstword $(MAKECMDGOALS)))

SELECTOR := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))

$(eval $(SELECTOR):;@:)
endif

all: compile test clean

%.elc:%.el
	$(EMACS) -batch -L . -l tests/test-bootstrap.el -f batch-byte-compile $(ELS)

compile:$(ELCS)

.PHONY: test

test:$(ELCS)

ifeq ($(SELECTOR),)
	$(EMACS) -Q --batch -L . $(addprefix -l , $(TEST_ELS)) -f ert-run-tests-batch-and-exit
else
	$(EMACS) -Q --batch -L . $(addprefix -l , $(TEST_ELS)) --eval "(ert-run-tests-batch-and-exit '$(SELECTOR))"
endif


help:
	@echo make
	@echo make compile
	@echo make test [SELECTOR]
	@echo make clean

clean:
	@rm -f *.elc
