SHELL := bash
.SHELLFLAGS := -ceuo pipefail
MAKEFLAGS += --no-print-directory
.ONESHELL:
#.SILENT:

PREVIOUS_VERSION=0.11
VERSION=0.11

PREVIOUS_REQUIRES=((emacs "29.4"))
REQUIRES=((emacs "29.4"))

PREVIOUS_LIB=${PWD}/total-recall-${PREVIOUS_VERSION}
LIB=${PWD}/total-recall-${VERSION}

PREVIOUS_ELISP_LIB=${LIB}/elisp
ELISP_LIB=${LIB}/elisp

PREVIOUS_PYTHON_LIB=${LIB}/python
PYTHON_LIB=${LIB}/python

FIXTURE=${PWD}/fixture

EL=${ELISP_LIB}/total-recall.el

PKG_EL=${ELISP_LIB}/total-recall-pkg.el

VENV=${PWD}/venv

VENV_ACTIVATE=${VENV}/bin/activate

VENV_DEV=${PYTHON_LIB}/dev-requirements.txt

VENV_PROD=${PYTHON_LIB}/prod-requirements.txt

define VENV_SETUP
export TOTAL_RECALL_VENV_ACTIVATE=${VENV_ACTIVATE}
export TOTAL_RECALL_PYTHON_LIB=${PYTHON_LIB}
endef

BIN=${LIB}/bin

.PHONY: tags
tags:
	fd -g '*.el' . ${ELISP_LIB} | etags -

.PHONY: version
version:
	sed -i "s|${PREVIOUS_VERSION}|${VERSION}|" ${PKG_EL}
	if [[ -d "${PREVIOUS_LIB}" ]]; then
	  if [[ "${PREVIOUS_LIB}" != "${LIB}" ]]; then
	    mv "${PREVIOUS_LIB}" "${LIB}"
	  fi
	fi
	echo "DONE $@"

.PHONY: requires
requires:
	sed -i 's|${PREVIOUS_REQUIRES}|${REQUIRES}|' ${PKG_EL} ${EL}
	echo "DONE $@"

.PHONY: dev
dev:
	export TOTAL_RECALL_ENV="dev"
	export TOTAL_RECALL_ROOT="${FIXTURE}"
	emacs -Q \
	--eval '(add-to-list (quote load-path) (expand-file-name "${ELISP_LIB}"))' \
	--eval '(require (quote total-recall))' \
	--eval '(setq debug-on-error t)' \
	--eval '(total-recall)' \
	echo "$@"

.PHONY: prod
prod:
	export TOTAL_RECALL_ENV="prod"
	export TOTAL_RECALL_ROOT="${HOME}/src/qa"
	emacs -Q \
	--eval '(add-to-list (quote load-path) (expand-file-name "${ELISP_LIB}"))' \
	--eval "(require 'total-recall)" \
	--eval "(total-recall)"
	echo "$@"

.PHONY: python-venv
python-venv:
	uv venv ${VENV} --python 3.13
	source ${VENV_ACTIVATE}
	uv pip install -r ${VENV_DEV} -r ${VENV_PROD}
	echo "DONE $@"

.PHONY: python-repl
python-repl: python-venv
	${VENV_SETUP}
	python
	echo "DONE $@"

.PHONY: graph
graph: python-venv
	${VENV_SETUP}
	${BIN}/graph

.PHONY: selector
selector: python-venv
	${VENV_SETUP}
	${BIN}/selector

.PHONY: format-python
format-python:
	source ${VENV_ACTIVATE}
	ruff format "${PYTHON_LIB}" &>/dev/null
	echo "DONE $@"

.PHONY: lint-python
lint-python: format-python
	source ${VENV_ACTIVATE}
	ruff check --fix "${PYTHON_LIB}" &>/dev/null
	echo "DONE $@"

.PHONY: lint-elisp
lint-elisp: version requires
	EMACS_INIT="(progn (require 'package) (package-initialize) (require 'package-lint))"
	emacs -Q --batch --eval "$$EMACS_INIT" -f package-lint-batch-and-exit "${EL}"
	echo "DONE $@"

.PHONY: lint
lint:
	${MAKE} lint-python
	${MAKE} lint-elisp
	echo "DONE $@"

.PHONY: compile
compile:
	for file in ${ELISP_LIB}/*.el; do
	  emacs -Q --batch --dir "${ELISP_LIB}" -f batch-byte-compile "$$file"
	done
	echo "DONE $@"

.PHONY: checkdoc
checkdoc:
	for file in ${ELISP_LIB}/*.el; do \
	  emacs -Q --batch \
	    --eval "(require 'checkdoc)" \
	    --eval "(checkdoc-file \"$$file\")"
	done
	echo "DONE $@"

.PHONY: dist
dist:
	${MAKE} clean
	mkdir -p ./dist
	tar -czf ./dist/total-recall-${VERSION}.tar.gz total-recall-${VERSION}
	zip -r ./dist/total-recall-${VERSION}.zip total-recall-${VERSION}
	echo "DONE $@"

.PHONY: all
all:
	${MAKE} lint
	${MAKE} compile
	${MAKE} checkdoc
	${MAKE} dist

.PHONY: clean
clean:
	fd -u -t f -e elc . ${ELISP_LIB} -x rm
	fd -u -t d -p __pycache__ ${PYTHON_LIB} -x rm -rf
	rm -rf ./dist
	rm -rf .ruff_cache
	rm -rf TAGS
	rm -rf venv
