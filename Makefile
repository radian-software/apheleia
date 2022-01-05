VERSION ?=
CMD ?=

EMACS ?= emacs

TAG ?= latest

# The order is important for compilation.
for_compile := *.el
for_checkdoc := *.el

.PHONY: help
help: ## Show this message
	@echo "usage:" >&2
	@grep -h "[#]# " $(MAKEFILE_LIST)	| \
		sed 's/^/  make /'		| \
		sed 's/:[^#]*[#]# /|/'		| \
		sed 's/%/LANG/'			| \
		column -t -s'|' >&2

.PHONY: lint
lint: compile checkdoc longlines fmt-lint ## Build project and run all linters

.PHONY: compile
compile: ## Check for byte-compiler errors
	@for file in $(for_compile); do \
	    echo "[compile] $$file" ;\
	    rm -f "$${file}c" ;\
	    $(EMACS) -Q --batch -L . -f batch-byte-compile $$file 2>&1 \
	        | grep -v "^Wrote" \
	        | grep . && exit 1 || true ;\
	done

.PHONY: checkdoc
checkdoc: ## Check for missing or poorly formatted docstrings
	@for file in $(for_checkdoc); do \
	    echo "[checkdoc] $$file" ;\
	    $(EMACS) -Q --batch \
	        --eval "(or (fboundp 'checkdoc-file) (kill-emacs))" \
	        --eval "(setq sentence-end-double-space nil)" \
	        --eval "(checkdoc-file \"$$file\")" 2>&1 \
	        | grep . && exit 1 || true ;\
	done

.PHONY: longlines
longlines: ## Check for long lines
	@scripts/check-line-length.bash

.PHONY: clean
clean: ## Remove build artifacts
	@echo "[clean]" *.elc
	@rm -f *.elc

.PHONY: docker
docker: ## Start a Docker shell; e.g. make docker VERSION=25.3
	@scripts/docker.bash "$(VERSION)" "$(CMD)"

.PHONY: fmt-build  # env vars: FORMATTERS, TAG
fmt-build: ## Build a Docker image with formatters installed
	@test/formatters/build-image.bash

.PHONY: fmt-docker  # env var: TAG
fmt-docker: ## Start a Docker shell for testing formatters
	@scripts/docker-run.bash -e FORMATTERS "apheleia-formatters:$(TAG)" "$(CMD)"

.PHONY: fmt-lint
fmt-lint: ## Do basic linting for formatter configuration
	@test/formatters/run-func.bash apheleia-ft-lint

.PHONY: fmt-check
fmt-changed: ## Get list of changed formatters on this PR
	@test/formatters/run-func.bash apheleia-ft-changed

.PHONY: fmt-test  # env var: FORMATTERS
fmt-test: ## Actually run formatter tests
	@test/formatters/run-func.bash apheleia-ft-test
