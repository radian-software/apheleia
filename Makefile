EMACS ?= emacs

# The order is important for compilation.
for_compile := *.el
for_checkdoc := *.el
for_longlines := $(wildcard *.el *.md *.yml) Makefile

.PHONY: all
all: compile checkdoc longlines

.PHONY: compile
compile:
	@for file in $(for_compile); do \
	    echo "[compile] $$file" ;\
	    $(EMACS) -Q --batch -L . -f batch-byte-compile $$file 2>&1 \
	        | grep -v "^Wrote" \
	        | grep . && exit 1 || true ;\
	done

.PHONY: checkdoc
checkdoc:
	@for file in $(for_checkdoc); do \
	    echo "[checkdoc] $$file" ;\
	    $(EMACS) -Q --batch \
	        --eval "(or (fboundp 'checkdoc-file) (kill-emacs))" \
	        --eval "(setq sentence-end-double-space nil)" \
	        --eval "(checkdoc-file \"$$file\")" 2>&1 \
	        | grep . && exit 1 || true ;\
	done

.PHONY: longlines
longlines:
	@echo "[longlines] $(for_longlines)"
	@for file in $(for_longlines); do \
	    cat "$$file" \
	        | sed '/[l]onglines-start/,/longlines-stop/d' \
	        | grep -E '.{80}' \
	        | grep -E -v 'https?://' \
	        | sed "s/^/$$file:long line: /" \
	        | grep . && exit 1 || true ;\
	done

.PHONY: clean
clean:
	@echo "[clean]" *.elc
	@rm -f *.elc
