;;; apheleia-formatters.el --- Apheleia formatters -*- lexical-binding: t -*-

;;; Commentary:

;; Formatter definitions for `apheleia'.

;;; Code:

(defcustom apheleia-formatters
  '((bean-format . ("bean-format"))
    (black . ("black" "-"))
    (brittany . ("brittany"))
    (caddyfmt . ("caddy" "fmt" "-"))
    (clang-format . ("clang-format"
                     "-assume-filename"
                     (or (buffer-file-name)
                         (cdr (assoc major-mode
                                     '((c-mode        . ".c")
                                       (c++-mode      . ".cpp")
                                       (cuda-mode     . ".cu")
                                       (protobuf-mode . ".proto"))))
                         ".c")))
    (crystal-tool-format . ("crystal" "tool" "format" "-"))
    (dart-format . ("dart" "format"))
    (elm-format . ("elm-format" "--yes" "--stdin"))
    (fish-indent . ("fish_indent"))
    (gofmt . ("gofmt"))
    (gofumpt . ("gofumpt"))
    (goimports . ("goimports"))
    (google-java-format . ("google-java-format" "-"))
    (isort . ("isort" "-"))
    (lisp-indent . apheleia-indent-lisp-buffer)
    (ktlint . ("ktlint" "--log-level=none" "--stdin" "-F"))
    (latexindent . ("latexindent" "--logfile=/dev/null"))
    (mix-format . ("mix" "format" "-"))
    (nixfmt . ("nixfmt"))
    (ocamlformat . ("ocamlformat" "-" "--name" filepath
                    "--enable-outside-detected-project"))
    (phpcs . ("apheleia-phpcs"))
    (prettier . (npx "prettier" "--stdin-filepath" filepath))
    (prettier-css
     . (npx "prettier" "--stdin-filepath" filepath "--parser=css"))
    (prettier-html
     . (npx "prettier" "--stdin-filepath" filepath "--parser=html"))
    (prettier-graphql
     . (npx "prettier" "--stdin-filepath" filepath "--parser=graphql"))
    (prettier-javascript
     . (npx "prettier" "--stdin-filepath" filepath "--parser=babel-flow"))
    (prettier-json
     . (npx "prettier" "--stdin-filepath" filepath "--parser=json"))
    (prettier-markdown
     . (npx "prettier" "--stdin-filepath" filepath "--parser=markdown"))
    (prettier-ruby
     . (npx "prettier" "--stdin-filepath" filepath "--parser=ruby"))
    (prettier-scss
     . (npx "prettier" "--stdin-filepath" filepath "--parser=scss"))
    (prettier-svelte
     . (npx "prettier" "--stdin-filepath" filepath "--parser=svelte"))
    (prettier-typescript
     . (npx "prettier" "--stdin-filepath" filepath "--parser=typescript"))
    (prettier-yaml
     . (npx "prettier" "--stdin-filepath" filepath "--parser=yaml"))
    (shfmt . ("shfmt" "-i" "4"))
    (stylua . ("stylua" "-"))
    (rustfmt . ("rustfmt" "--quiet" "--emit" "stdout"))
    (terraform . ("terraform" "fmt" "-")))
  "Alist of code formatting commands.
The keys may be any symbols you want, and the values are shell
commands, lists of strings and symbols, or a function symbol.

If the value is a function, the function will be called with
keyword arguments (see the implementation of
`apheleia--run-formatter-function' to see which). It should use
`cl-defun' with `&allow-other-keys' for forward compatibility.

Otherwise in Lisp code, the format of commands is similar to what
you pass to `make-process', except as follows.

Normally, the contents of the current buffer are passed to the
command on stdin, and the output is read from stdout. However, if
you use the symbol `file' as one of the elements of commands,
then the filename of the current buffer is substituted for
it. (Use `filepath' instead of `file' if you need the filename of
the current buffer, but you still want its contents to be passed
on stdin.)

If you instead use the symbol `input' as one of the elements of
commands, then the contents of the current buffer are written to
a temporary file and its name is substituted for `input'. Also,
if you use the symbol `output' as one of the elements of
commands, then it is substituted with the name of a temporary
file. In that case, it is expected that the command writes to
that file, and the file is then read into an Emacs buffer.

If you use the symbol `inplace' as one of the elements of the
list, then the contents of the current buffer are written to a
temporary file and its name is substituted for `inplace'.
However, unlike `input', it is expected that the formatter write
the formatted file back to the same file in place. In other
words, `inplace' is like `input' and `output' together.

If you use the symbol `npx' as one of the elements of commands,
then the first string element of the command list is resolved
inside node_modules/.bin if such a directory exists anywhere
above the current `default-directory'.

Any list elements that are not strings and not any of the special
symbols mentioned above will be evaluated when the formatter is
invoked, and spliced into the list. A form can evaluate either to
a string or to a list of strings.

The \"scripts/formatters\" subdirectory of the Apheleia source
repository is automatically prepended to $PATH (variable
`exec-path', to be specific) when invoking external formatters.
This is intended for internal use. If you would like to define
your own script, you can simply place it on your normal $PATH
rather than using this system."
  :type '(alist
          :key-type symbol
          :value-type
          (choice
           (repeat
            (choice
             (string :tag "Argument")
             (const :tag "Look for command in node_modules/.bin" npx)
             (const :tag "Name of file being formatted" filepath)
             (const :tag "Name of real file used for input" file)
             (const :tag "Name of temporary file used for input" input)
             (const :tag "Name of temporary file used for output" output)))
           (function :tag "Formatter function")))
  :group 'apheleia)

(defcustom apheleia-mode-alist
  '(;; php-mode has to come before cc-mode
    (php-mode . phpcs)
    ;; json-mode has to come before javascript-mode (aka js-mode)
    (json-mode . prettier-json)
    (json-ts-mode . prettier-json)
    ;; rest are alphabetical
    (bash-ts-mode . shfmt)
    (beancount-mode . bean-format)
    (c++-ts-mode . clang-format)
    (caddyfile-mode . caddyfmt)
    (cc-mode . clang-format)
    (c-mode . clang-format)
    (c-ts-mode . clang-format)
    (c++-mode . clang-format)
    (caml-mode . ocamlformat)
    (common-lisp-mode . lisp-indent)
    (crystal-mode . crystal-tool-format)
    (css-mode . prettier-css)
    (css-ts-mode . prettier-css)
    (dart-mode . dart-format)
    (elixir-mode . mix-format)
    (elixir-ts-mode . mix-format)
    (elm-mode . elm-format)
    (fish-mode . fish-indent)
    (go-mode . gofmt)
    (go-mod-ts-mode . gofmt)
    (go-ts-mode . gofmt)
    (graphql-mode . prettier-graphql)
    (haskell-mode . brittany)
    (html-mode . prettier-html)
    (java-mode . google-java-format)
    (java-ts-mode . google-java-format)
    (js3-mode . prettier-javascript)
    (js-mode . prettier-javascript)
    (js-ts-mode . prettier-javascript)
    (kotlin-mode . ktlint)
    (latex-mode . latexindent)
    (LaTeX-mode . latexindent)
    (lua-mode . stylua)
    (lisp-mode . lisp-indent)
    (nix-mode . nixfmt)
    (python-mode . black)
    (python-ts-mode . black)
    (ruby-mode . prettier-ruby)
    (ruby-ts-mode . prettier-ruby)
    (rustic-mode . rustfmt)
    (rust-mode . rustfmt)
    (rust-ts-mode . rustfmt)
    (scss-mode . prettier-scss)
    (svelte-mode . prettier-svelte)
    (terraform-mode . terraform)
    (TeX-latex-mode . latexindent)
    (TeX-mode . latexindent)
    (tsx-ts-mode . prettier-typescript)
    (tuareg-mode . ocamlformat)
    (typescript-mode . prettier-typescript)
    (typescript-ts-mode . prettier-typescript)
    (web-mode . prettier)
    (yaml-mode . prettier-yaml)
    (yaml-ts-mode . prettier-yaml))
  "Alist mapping major mode names to formatters to use in those modes.
This determines what formatter to use in buffers without a
setting for `apheleia-formatter'. The keys are major mode
symbols (matched against `major-mode' with `derived-mode-p') or
strings (matched against value of variable `buffer-file-name'
with `string-match-p'), and the values are symbols with entries
in `apheleia-formatters' (or equivalently, they are allowed
values for `apheleia-formatter'). Values can be a list of such
symnols causing each formatter in the list to be called one after
the other (with the output of the previous formatter).
Earlier entries in this variable take precedence over later ones.

Be careful when writing regexps to include \"\\'\" and to escape
\"\\.\" in order to properly match a file extension. For example,
to match \".jsx\" files you might use \"\\.jsx\\'\".

If a given mode derives from another mode (e.g. `php-mode' and
`cc-mode'), then ensure that the deriving mode comes before the mode
to derive from, as the list is interpreted sequentially."
  :type '(alist
          :key-type
          (choice (symbol :tag "Major mode")
                  (string :tag "Buffer name regexp"))
          :value-type
          (choice (symbol :tag "Formatter")
                  (repeat
                   (symbol :tag "Formatter"))))
  :group 'apheleia)

(provide 'apheleia-formatters)

;;; apheleia-formatters.el ends here
