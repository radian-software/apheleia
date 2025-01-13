SHELL := bash

VERSION ?=
CMD ?=

EMACS ?= emacs

TAG ?= latest

# The order is important for compilation.
for_compile := \
    apheleia-utils.el \
    apheleia-dp.el \
    apheleia-formatter-context.el \
    apheleia-log.el \
    apheleia-formatters.el \
    apheleia-rcs.el \
    apheleia.el
for_checkdoc := *.el
for_checkindent := *.el

.PHONY: help
help: ## Show this message
	@echo "usage:" >&2
	@grep -h "[#]# " $(MAKEFILE_LIST)	| \
		sed 's/^/  make /'		| \
		sed 's/:[^#]*[#]# /|/'		| \
		sed 's/%/LANG/'			| \
		column -t -s'|' >&2

.PHONY: lint
lint: compile checkdoc longlines checkindent fmt-lint ## Run all fast linters

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
		| (grep -v "Warning (emacs): \\?$$" ||:) \
		| (grep -v "Some lines are over 80 columns wide" ||:) \
	        | grep . && exit 1 || true ;\
	done

.PHONY: checkindent
checkindent: ## Ensure that indentation is correct
	@tmpdir="$$(mktemp -d)"; for file in $(for_checkindent); do \
	    echo "[checkindent] $$file" >&2; \
	    emacs -Q --batch \
		-l scripts/apheleia-indent.el \
	        --eval "(setq inhibit-message t)" \
	        --eval "(setq load-path \
	                      (append (list default-directory) load-path))" \
	        --eval "(load (expand-file-name \"apheleia.el\") nil t)" \
	        --eval "(find-file \"$$file\")" \
	        --eval "(indent-region (point-min) (point-max))" \
	        --eval "(write-file \"$$tmpdir/$$file\")"; \
	    (diff <(cat          "$$file" | nl -v1 -ba | \
                           sed "s/\t/: /" | sed "s/^ */$$file:/") \
	          <(cat "$$tmpdir/$$file" | nl -v1 -ba | \
                           sed "s/\t/: /" | sed "s/^ */$$file:/") ) \
	        | grep -F ">" | grep -o "[a-z].*" | grep . && exit 1 || true; \
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
	@COMMON=0 test/formatters/build-image.bash

.PHONY: fmt-build-common  # env var: TAG
fmt-build-common: ## Build a Docker image with just the common base
	@COMMON=1 test/formatters/build-image.bash

.PHONY: fmt-docker  # env var: TAG
fmt-docker: ## Start a Docker shell for testing formatters
	@scripts/docker-run.bash -e FORMATTERS "apheleia-formatters:$(TAG)" "$(CMD)"

APHELEIA_FT := -L test/formatters -l apheleia-ft

.PHONY: fmt-lint
fmt-lint: ## Do basic linting for formatter configuration
	@test/shared/run-func.bash apheleia-ft-lint $(APHELEIA_FT)

.PHONY: fmt-check
fmt-changed: ## Get list of changed formatters on this PR
	@test/shared/run-func.bash apheleia-ft-changed $(APHELEIA_FT)

.PHONY: fmt-test  # env var: FORMATTERS
fmt-test: ## Actually run formatter tests
	@test/shared/run-func.bash apheleia-ft-test $(APHELEIA_FT)

.PHONY: fmt-emacs  # env var: FILE
fmt-emacs: ## Start an Emacs instance for testing formatters
	@emacs -L . -L test/formatters -l apheleia-ft -f apheleia-global-mode $(FILE)

.PHONY: lint-changelog
lint-changelog: ## Report an error if the changelog wasn't updated
	@scripts/lint-changelog.bash

BUTTERCUP_VER := 1.34
BUTTERCUP := vendor/buttercup-$(BUTTERCUP_VER)

$(BUTTERCUP):
	@rm -rf $(BUTTERCUP) && mkdir -p $(BUTTERCUP)
	@curl -fsSL https://github.com/jorgenschaefer/emacs-buttercup/archive/refs/tags/v$(BUTTERCUP_VER).tar.gz -o $(BUTTERCUP).tar.gz
	@tar -xf $(BUTTERCUP).tar.gz --strip-components=1 -C $(BUTTERCUP)
	@rm $(BUTTERCUP).tar.gz

.PHONY: unit
unit: $(BUTTERCUP) ## Run unit tests
	@$(BUTTERCUP)/bin/buttercup test/unit -L $(BUTTERCUP) -L .

APHELEIA_IT := -L test/integration \
	--eval "(setq apheleia-log-debug-info t)" -l apheleia-it

.PHONY: integration
integration: ## Run integration tests
	@test/shared/run-func.bash apheleia-it-run-all-tests $(APHELEIA_IT)
