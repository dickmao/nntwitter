export EMACS ?= $(shell which emacs)
export CASK := $(shell which cask)
ifeq ($(CASK),)
$(error Please install CASK at https://cask.readthedocs.io/en/latest/guide/installation.html)
endif
CASK_DIR := $(shell $(CASK) package-directory || exit 1)
SRC = $(shell $(CASK) files)
UNLINTABLE=lisp/nntwitter-autoloads.el lisp/nntwitter-pkg.el
PKBUILD=2.3
VERSION = $(shell $(CASK) version)
ELCFILES = $(SRC:.el=.elc)
TESTS = $(shell ls tests/test*el)
TESTSSRC = $(TESTS) features/support/env.el tests/nntwitter-test.el
ELCTESTS = $(TESTSSRC:.el=.elc)

.DEFAULT_GOAL := test-compile

.PHONY: autoloads
autoloads: cask
	$(EMACS) -Q --batch --eval "(package-initialize)" --eval "(package-generate-autoloads \"nntwitter\" \"./lisp\")"

README.rst: README.in.rst lisp/nntwitter.el
	grep ';;' lisp/nntwitter.el \
	    | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	    | sed -e 's/^\s*;;*\s*//g' \
	    | tools/readme-sed.sh "COMMENTARY" README.in.rst > README.rst

.PHONY: cask
cask: $(CASK_DIR)

$(CASK_DIR): Cask
	$(CASK) install
	touch $(CASK_DIR)

.PHONY: clean
clean:
	$(CASK) clean-elc
	rm -f tests/log/*
	rm -rf tests/test-install

.PHONY: test-compile
test-compile: autoloads
	! ($(CASK) eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; $(CASK) clean-elc && exit $$ret)
	! ($(CASK) eval \
	      "(cl-letf (((symbol-function (quote cask-files)) (lambda (&rest _args) (mapcar (function symbol-name) (quote ($(TESTSSRC))))))) \
	          (let ((byte-compile-error-on-warn t)) (cask-cli/build)))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; rm -f $(ELCTESTS) && exit $$ret)

.PHONY: lint
lint: test-compile
# for f in $(filter-out $(UNLINTABLE),$(SRC)) ; do if ! sh -e tools/package-lint.sh $${f} ; then exit 2 ; fi ; done
	bash -ex tools/melpazoid.sh

define SET_GITHUB_REPOSITORY =
ifeq ($(GITHUB_REPOSITORY),)
	GITHUB_REPOSITORY := $(shell git config user.name)/$(shell basename `git rev-parse --show-toplevel`)
endif
endef

define SET_GITHUB_HEAD_REF =
ifeq ($(GITHUB_HEAD_REF),)
GITHUB_HEAD_REF := $(shell git rev-parse --abbrev-ref HEAD)
endif
endef

define SET_GITHUB_SHA =
ifeq ($(GITHUB_SHA),)
GITHUB_SHA := $(shell if git show-ref --quiet --verify origin/$(GITHUB_HEAD_REF) ; then git rev-parse origin/$(GITHUB_HEAD_REF) ; fi)
endif
endef

.PHONY: test-install-vars
test-install-vars:
	$(eval $(call SET_GITHUB_REPOSITORY))
	$(eval $(call SET_GITHUB_HEAD_REF))
	$(eval $(call SET_GITHUB_SHA))
	@true

.PHONY: test-install
test-install: test-install-vars
	mkdir -p tests/test-install
	if [ ! -s "tests/test-install/$(PKBUILD).tar.gz" ] ; then \
	  cd tests/test-install ; curl -sLOk https://github.com/melpa/package-build/archive/$(PKBUILD).tar.gz ; fi
	cd tests/test-install ; tar xfz $(PKBUILD).tar.gz
	cd tests/test-install ; rm -f $(PKBUILD).tar.gz
	cd tests/test-install/package-build-$(PKBUILD) ; make -s loaddefs
	mkdir -p tests/test-install/recipes
	cd tests/test-install/recipes ; curl -sfLOk https://raw.githubusercontent.com/melpa/melpa/master/recipes/nntwitter || cp -f ../../../tools/recipe ./nntwitter
	! ( $(EMACS) -Q --batch -L tests/test-install/package-build-$(PKBUILD) \
	--eval "(require 'package-build)" \
	--eval "(require 'subr-x)" \
	--eval "(package-initialize)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	--eval "(package-refresh-contents)" \
	--eval "(setq rcp (package-recipe-lookup \"nntwitter\"))" \
	--eval "(unless (file-exists-p package-build-archive-dir) \
	           (make-directory package-build-archive-dir))" \
	--eval "(let* ((my-repo \"$(GITHUB_REPOSITORY)\") \
	               (my-branch \"$(GITHUB_HEAD_REF)\") \
	               (my-commit \"$(GITHUB_SHA)\")) \
	           (oset rcp :repo my-repo) \
	           (oset rcp :branch my-branch) \
	           (oset rcp :commit my-commit))" \
	--eval "(package-build--package rcp (package-build--checkout rcp))" \
	--eval "(package-install-file (car (file-expand-wildcards (concat package-build-archive-dir \"nntwitter*.tar\"))))" 2>&1 | egrep -ia "error: |fatal" )

.PHONY: test-unit
test-unit:
	$(CASK) exec ert-runner -L . -L tests tests/test*.el

.PHONY: test
test: test-compile test-unit test-int

.PHONY: test-int
test-int:
	rm -f tests/.newsrc.eld
	$(CASK) exec ecukes --debug --reporter magnars

.PHONY: dist-clean
dist-clean:
	rm -rf dist

.PHONY: dist
dist: dist-clean
	$(CASK) package

.PHONY: install
install: dist
	$(EMACS) -Q --batch --eval "(package-initialize)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	  --eval "(package-refresh-contents)" \
	  --eval "(package-install-file (car (file-expand-wildcards \"dist/nntwitter*.tar\")))"
