;;; apheleia-formatters.el --- Run formatters -*- lexical-binding: t -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module defines a series of functions for running a formatter process
;; or formatter function and generating a RCS patch from the result.

;;; Code:

(require 'apheleia-formatter-context)
(require 'apheleia-log)
(require 'apheleia-utils)

(require 'cl-lib)
(require 'map)
(require 'subr-x)

(eval-when-compile
  (require 'rx))

(defcustom apheleia-formatters
  '((astyle . ("astyle" (apheleia-formatters-locate-file
                         "--options" ".astylerc")))
    (asmfmt . ("asmfmt"))
    (bean-format . ("bean-format" input))
    (beautysh . ("beautysh"
                 (apheleia-formatters-indent
                  "--tab" "--indent-size" 'sh-basic-offset)
                 "-"))
    (black . ("black"
              (when (apheleia-formatters-extension-p "pyi") "--pyi")
              (apheleia-formatters-fill-column "--line-length")
              "-"))
    (brittany . ("brittany"))
    (buildifier . ("buildifier"))
    (caddyfmt . ("caddy" "fmt" "-"))
    (clang-format . ("clang-format"
                     "-assume-filename"
                     (or (apheleia-formatters-local-buffer-file-name)
                         (apheleia-formatters-mode-extension)
                         ".c")))
    (cljfmt . ("cljfmt" "fix" "-"))
    (cmake-format . ("cmake-format" "-"))
    (crystal-tool-format . ("crystal" "tool" "format" "-"))
    (css-beautify "css-beautify" "--file" "-" "--end-with-newline"
                  (apheleia-formatters-indent
                   "--indent-with-tabs" "--indent-size"))
    (dart-format . ("dart" "format"))
    (denofmt . ("deno" "fmt" "-"))
    (denofmt-js . ("deno" "fmt" "-" "--ext" "js"))
    (denofmt-json . ("deno" "fmt" "-" "--ext" "json"))
    (denofmt-jsonc . ("deno" "fmt" "-" "--ext" "jsonc"))
    (denofmt-jsx . ("deno" "fmt" "-" "--ext" "jsx"))
    (denofmt-md . ("deno" "fmt" "-" "--ext" "md"))
    (denofmt-ts . ("deno" "fmt" "-" "--ext" "ts"))
    (denofmt-tsx . ("deno" "fmt" "-" "--ext" "tsx"))
    (dhall-format . ("dhall" "format"))
    (docformatter . ("apheleia-docformatter" inplace))
    (dprint . ("dprint" "fmt" "--stdin" filepath))
    (elm-format . ("elm-format" "--yes" "--stdin"))
    (fish-indent . ("fish_indent"))
    (fourmolu . ("fourmolu"))
    (gawk . ("gawk" "-f" "-" "--pretty-print=-"))
    (gleam . ("gleam" "format" "--stdin"))
    (gofmt . ("gofmt"))
    (gofumpt . ("gofumpt"))
    (goimports . ("goimports"))
    (google-java-format . ("google-java-format" "-"))
    (hclfmt . ("hclfmt"))
    (html-beautify "html-beautify" "--file" "-" "--end-with-newline"
                   (apheleia-formatters-indent
                    "--indent-with-tabs" "--indent-size"))
    (html-tidy "tidy"
               "--quiet" "yes"
               "--tidy-mark" "no"
               "--vertical-space" "yes"
               "-indent"
               (when (derived-mode-p 'nxml-mode)
                 "-xml")
               (apheleia-formatters-indent
                "--indent-with-tabs" "--indent-spaces")
               (apheleia-formatters-fill-column "-wrap"))
    (isort . ("isort" "-"))
    (js-beautify "js-beautify" "--file" "-" "--end-with-newline"
                 (apheleia-formatters-indent
                  "--indent-with-tabs" "--indent-size"))
    (jq "jq" "." "-M"
        (apheleia-formatters-indent "--tab" "--indent"))
    (lisp-indent . apheleia-indent-lisp-buffer)
    (ktlint . ("ktlint" "--log-level=none" "--stdin" "-F" "-"))
    (latexindent . ("latexindent" "--logfile=/dev/null"))
    (mix-format . ("apheleia-from-project-root"
                   ".formatter.exs" "apheleia-mix-format" filepath))
    (nixfmt . ("nixfmt"))
    (ocamlformat . ("ocamlformat" "-" "--name" filepath
                    "--enable-outside-detected-project"))
    (ocp-indent . ("ocp-indent"))
    (ormolu . ("ormolu" "--stdin-input-file" filepath))
    (perltidy . ("perltidy" "--quiet" "--standard-error-output"
                 (apheleia-formatters-indent "-t" "-i")
                 (apheleia-formatters-fill-column "-l")))
    (pgformatter . ("pg_format"
                    (apheleia-formatters-indent "--tabs" "--spaces" 'tab-width)
                    (apheleia-formatters-fill-column "--wrap-limit")))
    (phpcs . ("apheleia-phpcs"))
    (prettier
     . ("apheleia-npx" "prettier" "--stdin-filepath" filepath
        (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
    (prettier-css
     . ("apheleia-npx" "prettier" "--stdin-filepath" filepath
        "--parser=css"
        (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
    (prettier-html
     . ("apheleia-npx" "prettier" "--stdin-filepath" filepath
        "--parser=html"
        (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
    (prettier-graphql
     . ("apheleia-npx" "prettier" "--stdin-filepath" filepath
        "--parser=graphql"
        (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
    (prettier-javascript
     . ("apheleia-npx" "prettier" "--stdin-filepath" filepath
        "--parser=babel-flow"
        (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
    (prettier-json
     . ("apheleia-npx" "prettier" "--stdin-filepath" filepath
        "--parser=json"
        (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
    (prettier-markdown
     . ("apheleia-npx" "prettier" "--stdin-filepath" filepath
        "--parser=markdown"
        (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
    (prettier-ruby
     . ("apheleia-npx" "prettier" "--stdin-filepath" filepath
        "--plugin=@prettier/plugin-ruby" "--parser=ruby"
        (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
    (prettier-scss
     . ("apheleia-npx" "prettier" "--stdin-filepath" filepath
        "--parser=scss"
        (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
    (prettier-svelte
     . ("apheleia-npx" "prettier" "--stdin-filepath" filepath
        "--plugin=prettier-plugin-svelte" "--parser=svelte"
        (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
    (prettier-typescript
     . ("apheleia-npx" "prettier" "--stdin-filepath" filepath
        "--parser=typescript"
        (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
    (prettier-yaml
     . ("apheleia-npx" "prettier" "--stdin-filepath" filepath
        "--parser=yaml"
        (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
    (purs-tidy . ("apheleia-npx" "purs-tidy" "format"))
    (pyang . ("pyang" "--ignore-errors" "-f" "yang"))
    (robotidy . ("robotidy" "--no-color" "-"
                 (apheleia-formatters-indent nil "--indent")
                 (apheleia-formatters-fill-column "--line-length")))
    (python3-json
     . ("python3" "-m" "json.tool"
        (apheleia-formatters-indent "--tab" "--indent")))
    (rubocop . ("rubocop" "--stdin" filepath "-a"
                "--stderr" "--format" "quiet" "--fail-level" "fatal"))
    (ruby-standard . ("standardrb" "--stdin" filepath "--fix" "--stderr"
                      "--format" "quiet" "--fail-level" "fatal"))
    (ruby-syntax-tree . ("apheleia-from-project-root"
                         ".streerc" "stree" "format" filepath))
    (ruff . ("ruff" "format"
             "--silent"
             (apheleia-formatters-fill-column "--line-length")
             "--stdin-filename" filepath
             "-"))
    (ruff-isort . ("ruff" "check"
                   "-n"
                   "--select" "I"
                   "--fix" "--fix-only"
                   "--stdin-filename" filepath
                   "-"))
    (snakefmt . ("snakefmt"
                 (apheleia-formatters-fill-column "--line-length")
                 "-"))
    (shfmt . ("shfmt"
              "-filename" filepath
              "-ln" (cl-case (bound-and-true-p sh-shell)
                      (sh "posix")
                      (t "bash"))
              (when apheleia-formatters-respect-indent-level
                (list
                 "-i" (number-to-string
                       (cond
                        (indent-tabs-mode 0)
                        ((boundp 'sh-basic-offset)
                         sh-basic-offset)
                        (t 4)))))
              "-"))
    (rufo . ("rufo" "--filename" filepath "--simple-exit"))
    (stylua . ("stylua" "-"))
    (rustfmt . ("rustfmt" "--quiet" "--emit" "stdout"))
    (terraform . ("terraform" "fmt" "-"))
    (treefmt . ("treefmt" "--stdin" filepath))
    (typstyle . ("typstyle"))
    (vfmt . ("v" "fmt"))
    (xmllint . ("xmllint" "--format" "-"))
    (yapf . ("yapf"))
    (yq-csv . ("yq" "--prettyPrint" "--no-colors"
               "--input-format" "csv" "--output-format" "csv"))
    (yq-json . ("yq" "--prettyPrint" "--no-colors"
                "--input-format" "json" "--output-format" "json"
                (apheleia-formatters-indent nil "--indent")))
    (yq-properties . ("yq" "--prettyPrint" "--no-colors"
                      "--input-format" "props" "--output-format" "props"))
    (yq-tsv . ("yq" "--prettyPrint" "--no-colors"
               "--input-format" "tsv" "--output-format" "tsv"))
    (yq-xml . ("yq" "--prettyPrint" "--no-colors"
               "--input-format" "xml" "--output-format" "xml"
               (apheleia-formatters-indent nil "--indent")))
    (yq-yaml . ("yq" "--prettyPrint" "--no-colors" "--no-doc"
                "--input-format" "yaml" "--output-format" "yaml"
                (apheleia-formatters-indent nil "--indent")))
    (zig-fmt . ("zig" "fmt" "--stdin")))
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

\(However, instead of using `npx', consider using
\"apheleia-npx\", which is a built-in script that will replicate
the effect, but will also work with Yarn PNP projects and other
npm project types that may exist in the future.)

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
             (const :tag "TODO: docstring" inplace)
             (const :tag "Name of file being formatted" filepath)
             (const :tag "Name of real file used for input" file)
             (const :tag "Name of temporary file used for input" input)
             (const :tag "Name of temporary file used for output" output)))
           (function :tag "Formatter function")))
  :group 'apheleia)

(defcustom apheleia-mode-alist
  '(;; Alphabetical please
    (asm-mode . asmfmt)
    (awk-mode . gawk)
    (bash-ts-mode . shfmt)
    (bazel-mode . buildifier)
    (beancount-mode . bean-format)
    (c++-ts-mode . clang-format)
    (caddyfile-mode . caddyfmt)
    (cc-mode . clang-format)
    (c-mode . clang-format)
    (c-ts-mode . clang-format)
    (c++-mode . clang-format)
    (caml-mode . ocamlformat)
    (clojure-mode . cljfmt)
    (clojure-ts-mode . cljfmt)
    (cmake-mode . cmake-format)
    (cmake-ts-mode . cmake-format)
    (common-lisp-mode . lisp-indent)
    (conf-toml-mode . dprint)
    (cperl-mode . perltidy)
    (crystal-mode . crystal-tool-format)
    (css-mode . prettier-css)
    (css-ts-mode . prettier-css)
    (dart-mode . dart-format)
    (dart-ts-mode . dart-format)
    (dockerfile-mode . dprint)
    (elixir-mode . mix-format)
    (elixir-ts-mode . mix-format)
    (elm-mode . elm-format)
    (emacs-lisp-mode . lisp-indent)
    (fish-mode . fish-indent)
    (gleam-ts-mode . gleam)
    (go-mode . gofmt)
    (go-ts-mode . gofmt)
    (graphql-mode . prettier-graphql)
    (haskell-mode . ormolu)
    (hcl-mode . hclfmt)
    (html-mode . prettier-html)
    (html-ts-mode . prettier-html)
    (java-mode . google-java-format)
    (java-ts-mode . google-java-format)
    (jinja2-mode . nil)
    (js3-mode . prettier-javascript)
    (js-json-mode . prettier-json)
    (js-mode . prettier-javascript)
    (js-ts-mode . prettier-javascript)
    (json-mode . prettier-json)
    (json-ts-mode . prettier-json)
    (kotlin-mode . ktlint)
    (kotlin-ts-mode . ktlint)
    (latex-mode . latexindent)
    (LaTeX-mode . latexindent)
    (lua-mode . stylua)
    (lua-ts-mode . stylua)
    (lisp-mode . lisp-indent)
    ;; markdown-mode not included because so many people format
    ;; markdown code in so many different ways and we don't want to
    ;; try imposing a standard by default
    (nasm-mode . asmfmt)
    (nix-mode . nixfmt)
    (perl-mode . perltidy)
    (php-mode . phpcs)
    (purescript-mode . purs-tidy)
    (python-mode . black)
    (python-ts-mode . black)
    (robot-mode . robotidy)
    (ruby-mode . prettier-ruby)
    (ruby-ts-mode . prettier-ruby)
    (rustic-mode . rustfmt)
    (rust-mode . rustfmt)
    (rust-ts-mode . rustfmt)
    (snakemake-mode . snakefmt)
    (scss-mode . prettier-scss)
    (sql-mode . pgformatter)
    (svelte-mode . prettier-svelte)
    (terraform-mode . terraform)
    (TeX-latex-mode . latexindent)
    (TeX-mode . latexindent)
    (tsx-ts-mode . prettier-typescript)
    (tuareg-mode . ocamlformat)
    (typescript-mode . prettier-typescript)
    (typescript-ts-mode . prettier-typescript)
    (typst-mode . typstyle)
    (typst-ts-mode . typstyle)
    (v-mode . vfmt)
    (web-mode . prettier)
    (yaml-mode . prettier-yaml)
    (yaml-ts-mode . prettier-yaml)
    (yang-mode . pyang)
    (zig-mode . zig-fmt)
    (zig-ts-mode . zig-fmt))
  "Alist mapping major mode names to formatters to use in those modes.
This determines what formatter to use in buffers without a
setting for `apheleia-formatter'. The keys are major mode
symbols (matched against `major-mode' with `derived-mode-p') or
strings (matched against value of variable `buffer-file-name'
with `string-match-p'), and the values are symbols with entries
in `apheleia-formatters' (or equivalently, they are allowed
values for `apheleia-formatter'). Values can be a list of such
symbols causing each formatter in the list to be called one after
the other (with the output of the previous formatter).
Earlier entries in this variable take precedence over later ones.

Be careful when writing regexps to include \"\\'\" and to escape
\"\\.\" in order to properly match a file extension. For example,
to match \".jsx\" files you might use \"\\.jsx\\'\".

If a given mode derives from another mode (e.g. `php-mode' and
`cc-mode'), then whichever entry in the alist is more specific
will apply. In the case that multiple modes match
`derived-mode-p' for the current buffer but neither derives from
the other, whichever entry comes first will be used.

The value for a major mode can be nil (equivalently, an empty
list). This means to use no formatter. This can be helpful in
case your major mode derives from a more general one, but you
don't want the more general formatter to apply to the derived
mode."
  :type '(alist
          :key-type
          (choice (symbol :tag "Major mode")
                  (string :tag "Buffer name regexp"))
          :value-type
          (choice (symbol :tag "Formatter")
                  (repeat
                   (symbol :tag "Formatter"))))
  :group 'apheleia)

(defun apheleia-mhtml-mode-predicate ()
  "Return `mhtml-mode' if the user is in that mode.
This checks text properties because `mhtml-mode' sets
`major-mode' to different values depending on where the user is
in the buffer."
  (when (get-text-property
         (if (and (eobp) (not (bobp)))
             (1- (point))
           (point))
         'mhtml-submode)
    #'mhtml-mode))

;;;###autoload
(defcustom apheleia-mode-predicates '(apheleia-mhtml-mode-predicate)
  "List of predicates that check for sneaky major modes.
Sometimes a major mode will set `major-mode' to something other
than itself, making it hard to correctly detect what major mode
is active. In such cases you can add a predicate to this list to
handle it. Predicates take no arguments, are run in the current
buffer, and should return the name of a mode if one is detected.
If all the predicates return nil, or if there aren't any in the
list, then only the value of `major-mode' is used to determine
the major mode. The detected major mode affects the selection
from `apheleia-mode-alist'."
  :type '(repeat function)
  :group 'apheleia)

(defcustom apheleia-formatter-exited-hook nil
  "Abnormal hook run after a formatter has finished running.
Must accept arbitrary keyword arguments. The following arguments
are defined at present:

`:formatter' - The symbol for the formatter that was run.

`:error' - Non-nil if the formatter failed, nil if it succeeded.

`:log' - The log buffer for that formatter, or nil if there is
none (e.g., because logging is not enabled).

This hook is run before `apheleia-after-format-hook', and may be
run multiple times if `apheleia-mode-alist' configures multiple
formatters to run in a chain, with one run per formatter."
  :type 'hook
  :group 'apheleia)

(defcustom apheleia-remote-algorithm 'cancel
  "How `apheleia' should process remote files/buffers.
Set to `cancel' to immediately fail whenever you try to format a remote
buffer.

Set to `remote' to make apheleia spawn the process and any other temporary
files on the same remote machine the buffer is on. Note due to restrictions
with `tramp' when this option is set `apheleia' will run any formatters
synchronously, meaning Emacs will block until formatting the buffer finishes.
For more information see:
https://www.mail-archive.com/tramp-devel@gnu.org/msg05623.html

Set to `local' to make `apheleia' run the formatter on the current machine
and then write the formatted output back to the remote machine. Note some
features of `apheleia' (such as `file' in `apheleia-formatters') is not
compatible with this option and formatters relying on them will crash."
  :type '(choice (const :tag "Run the formatter on the local machine" local)
                 (const :tag "Run the formatter on the remote machine" remote)
                 (const :tag "Disable formatting for remote buffers" cancel))
  :group 'apheleia)

(defvar-local apheleia--current-process nil
  "Current process that Apheleia is running, or nil.
Keeping track of this helps avoid running more than one process
at once.")

(cl-defun apheleia--make-process
    (&key name stdin stdout stderr command
          remote noquery connection-type callback)
  "Helper to run a formatter process asynchronously.
This starts a formatter process using COMMAND and then connects
STDIN, STDOUT and STDERR buffers to the processes different
streams. Once the process is finished CALLBACK will be invoked
with the exit-code of the formatter process as well as a boolean
saying whether the process was interrupted before completion.
REMOTE if supplied will be passed as the FILE-HANDLER argument to
`make-process'.

See `make-process' for a description of the NAME and NOQUERY
arguments."
  (apheleia--log
   'process "Using make-process to create process %s with %S" name command)
  (let ((proc
         (make-process
          :name name
          :buffer stdout
          :stderr stderr
          :command command
          :file-handler remote
          :noquery noquery
          :connection-type connection-type
          :sentinel
          (lambda (proc _event)
            (unless (process-live-p proc)
              (funcall
               callback
               (process-exit-status proc)
               (process-get proc :interrupted)))))))
    (set-process-sentinel (get-buffer-process stderr) #'ignore)
    (when stdin
      (apheleia--log
       'process
       "Sending %d bytes to stdin of process %s" (buffer-size stdin) name)
      (set-process-coding-system
       proc
       nil
       (buffer-local-value 'buffer-file-coding-system stdin))
      (process-send-string
       proc
       (with-current-buffer stdin
         (buffer-string))))
    (process-send-eof proc)
    proc))

(cl-defun apheleia--call-process
    (&key name stdin stdout stderr command
          remote noquery connection-type callback)
  "Helper to synchronously run a formatter process.
This function essentially runs COMMAND synchronously passing STDIN
as standard input and saving output to the STDOUT and STDERR buffers.
Once the process is finished CALLBACK will be invoked with the exit
code (see `process-exit-status') of the process.

This function accepts all the same arguments as `apheleia--make-process'
for simplicity, however some may not be used. This includes: NAME,
NO-QUERY, and CONNECTION-TYPE."
  (ignore name noquery connection-type)
  (let* ((run-on-remote (and (eq apheleia-remote-algorithm 'remote)
                             remote))
         (stderr-file (apheleia--make-temp-file run-on-remote "apheleia"))
         (args
          (append
           (list
            ;; argv[0]
            (car command)
            ;; If stdin we don't delete the STDIN buffer text with
            ;; `call-process-region'. Otherwise we send no INFILE
            ;; argument to `call-process'.
            (not stdin)
            ;; stdout buffer and stderr file. `call-process' cannot
            ;; capture stderr into a separate buffer, the best we can
            ;; do is save and read from a file.
            `(,stdout ,stderr-file)
            ;; Do not re/display stdout as output is received.
            nil)
           ;; argv[1:]
           (cdr command))))
    (apheleia--log
     'process "Sending stderr for process %s to tempfile %s"
     name stderr-file)
    (unwind-protect
        (let ((exit-status
               (cl-letf* ((message (symbol-function #'message))
                          ((symbol-function #'message)
                           (lambda (format-string &rest args)
                             (unless (string-prefix-p "Renaming" (car args))
                               (apply message format-string args)))))
                 (cond
                  ((and run-on-remote stdin)
                   ;; There's no call-process variant for this, we'll have to
                   ;; copy STDIN to a remote temporary file, create a subshell
                   ;; on the remote that runs the formatter and passes the temp
                   ;; file as stdin and then deletes it.
                   (let* ((remote-stdin
                           (apheleia--make-temp-file
                            run-on-remote "apheleia-stdin"))
                          ;; WARN: This assumes a POSIX compatible shell.
                          (shell
                           (or (bound-and-true-p tramp-default-remote-shell)
                               "sh"))
                          (shell-command
                           (concat
                            (mapconcat #'shell-quote-argument command " ")
                            " < "
                            (shell-quote-argument
                             (apheleia-formatters-local-buffer-file-name
                              remote-stdin)))))
                     (unwind-protect
                         (progn
                           (with-current-buffer stdin
                             (apheleia--write-region-silently
                              nil nil remote-stdin))
                           (apheleia--log
                            'process
                            "Using process-file to create process %s with %S"
                            name (list shell "-c" shell-command))
                           (process-file
                            shell nil (nth 2 args) nil "-c" shell-command))
                       (delete-file remote-stdin))))
                  (stdin
                   (apheleia--log
                    'process
                    "Using call-process-region to create process %s with %S"
                    name command)
                   (with-current-buffer stdin
                     (apply #'call-process-region
                            (point-min) (point-max) args)))
                  (run-on-remote
                   (apheleia--log
                    'process
                    "Using process-file to create process %s with %S"
                    name command)
                   (apply #'process-file args))
                  (t
                   (apheleia--log
                    'process
                    "Using process-file to create process %s with %S"
                    name command)
                   (apply #'call-process args))))))
          ;; Save stderr from STDERR-FILE back into the STDERR buffer.
          (with-current-buffer stderr
            (insert-file-contents stderr-file))
          ;; I don't think it's possible to get here if the process
          ;; was interrupted, since we were running it synchronously,
          ;; so it should be ok to assume we pass nil to the callback.
          (funcall callback exit-status nil)
          ;; We return nil because there's no live process that can be
          ;; returned.
          nil)
      (delete-file stderr-file))))

(cl-defun apheleia--execute-formatter-process
    (&key ctx callback ensure exit-status)
  "Wrapper for `make-process' that behaves a bit more nicely.
CTX is a formatter process context (see
`apheleia-formatter--context'). CALLBACK is invoked with two
arguments. The first is an error or nil. The second is the buffer
containing the text from stdout, when the process terminates (if
it succeeds). ENSURE is a callback that's invoked whether the
process exited successfully or not. EXIT-STATUS is a function
which is called with the exit status of the command; it should
return non-nil to indicate that the command succeeded. If
EXIT-STATUS is omitted, then the command succeeds provided that
its exit status is 0."
  (apheleia--log
   'process "Trying to execute formatter process %s with %S"
   (apheleia-formatter--name ctx)
   `(,(apheleia-formatter--arg1 ctx)
     ,@(apheleia-formatter--argv ctx)))
  (when (process-live-p apheleia--current-process)
    (apheleia--log
     'process "Interrupting an existing process %S" apheleia--current-process)
    (message "Interrupting %s" apheleia--current-process)
    (process-put apheleia--current-process :interrupted t)
    (interrupt-process apheleia--current-process)
    (accept-process-output apheleia--current-process 0.1 nil 'just-this-one)
    (when (process-live-p apheleia--current-process)
      (kill-process apheleia--current-process)))
  (let* ((name (file-name-nondirectory (apheleia-formatter--arg1 ctx)))
         (stdout (generate-new-buffer
                  (format " *apheleia-%s-stdout*" name)))
         (stderr (generate-new-buffer
                  (format " *apheleia-%s-stderr*" name)))
         (log-name (apheleia-log--buffer-name name)))
    (condition-case-unless-debug e
        (progn
          (setq apheleia--current-process
                (funcall
                 (if (apheleia-formatter--remote ctx)
                     #'apheleia--call-process
                   #'apheleia--make-process)
                 :name (format "apheleia-%s" name)
                 :stdin (apheleia-formatter--stdin ctx)
                 :stdout stdout
                 :stderr stderr
                 :command `(,(apheleia-formatter--arg1 ctx)
                            ,@(apheleia-formatter--argv ctx))
                 :remote (apheleia-formatter--remote ctx)
                 :connection-type 'pipe
                 :noquery t
                 :callback
                 (lambda (proc-exit-status proc-interrupted)
                   (apheleia--log
                    'process
                    "Process %s exited with status %S%s"
                    name
                    proc-exit-status
                    (if proc-interrupted
                        " (interrupted)"
                      " (not interrupted)"))
                   (setf (apheleia-formatter--exit-status ctx)
                         proc-exit-status)
                   (let ((exit-ok (and
                                   (not proc-interrupted)
                                   (funcall
                                    (or exit-status #'zerop)
                                    (apheleia-formatter--exit-status ctx)))))
                     ;; Append standard-error from current formatter
                     ;; to log buffer when
                     ;; `apheleia-log-only-errors' is nil or the
                     ;; formatter failed. Every process output is
                     ;; delimited by a line-feed character.
                     (unless (and exit-ok apheleia-log-only-errors)
                       (apheleia-log--formatter-result
                        ctx
                        log-name
                        exit-ok
                        (buffer-local-value 'default-directory stdout)
                        (with-current-buffer stderr
                          (string-trim (buffer-string)))))
                     (when (apheleia-formatter--name ctx)
                       (apheleia--log
                        'hook
                        "Invoking apheleia-formatter-exited-hook")
                       (run-hook-with-args
                        'apheleia-formatter-exited-hook
                        :formatter (apheleia-formatter--name ctx)
                        :error (not exit-ok)
                        :log (get-buffer log-name)))
                     (unwind-protect
                         (if exit-ok
                             (funcall callback nil stdout)
                           (let ((errmsg
                                  (format
                                   (concat
                                    "Failed to run %s: exit status %s "
                                    "(see %s %s)")
                                   (apheleia-formatter--arg1 ctx)
                                   proc-exit-status
                                   (if (string-prefix-p " " log-name)
                                       "hidden buffer"
                                     "buffer")
                                   (string-trim log-name))))
                             (message "%s" errmsg)
                             (when noninteractive
                               (message
                                "%s"
                                (concat
                                 "(log buffer shown"
                                 " below in batch mode)\n"
                                 (with-current-buffer log-name
                                   (buffer-string)))))
                             (funcall
                              callback (cons 'error errmsg) nil)))
                       (when ensure
                         (funcall ensure))
                       (ignore-errors
                         (kill-buffer stdout))
                       (ignore-errors
                         (kill-buffer stderr))))))))
      (error
       (ignore-errors
         (kill-buffer stdout))
       (ignore-errors
         (kill-buffer stderr))
       (message "Failed to run %s: %s" name (error-message-string e))))))

(defun apheleia--write-region-silently
    (start end filename &optional
           append visit lockname mustbenew write-region)
  "Like `write-region', but silent.
START, END, FILENAME, APPEND, VISIT, LOCKNAME, and MUSTBENEW are
as in `write-region'. WRITE-REGION is used instead of the actual
`write-region' function, if provided."
  (funcall (or write-region #'write-region)
           start end filename append 0 lockname mustbenew)
  (when (or (eq visit t) (stringp visit))
    (setq buffer-file-name (if (eq visit t)
                               filename
                             visit))
    (set-visited-file-modtime)
    (set-buffer-modified-p nil)))

(defun apheleia--save-buffer-silently ()
  "Save the current buffer to its backing file, silently."
  (cl-letf* ((write-region (symbol-function #'write-region))
             ((symbol-function #'write-region)
              (lambda (start end filename &optional
                             append visit lockname mustbenew)
                (apheleia--write-region-silently
                 start end filename append visit
                 lockname mustbenew write-region)))
             (message (symbol-function #'message))
             ((symbol-function #'message)
              (lambda (format &rest args)
                (unless (equal format "Saving file %s...")
                  (apply message format args))))
             ;; Avoid triggering `after-set-visited-file-name-hook',
             ;; which can have various undesired effects in particular
             ;; major modes. Unfortunately, `write-file' triggers this
             ;; hook unconditionally even if the filename was not
             ;; changed, hence this hack :/
             (run-hooks (symbol-function #'run-hooks))
             ((symbol-function #'run-hooks)
              (lambda (&rest args)
                (unless (equal args '(after-set-visited-file-name-hook))
                  (apply run-hooks args)))))
    (save-buffer)))

(defun apheleia--make-temp-file (remote prefix &optional dir-flag suffix)
  "Create a temporary file optionally on a remote machine.
This function calls `make-temp-file' or `make-nearby-temp-file' depending on
the value of REMOTE.

See `make-temp-file' for a description of PREFIX, DIR-FLAG, and SUFFIX."
  (funcall
   (if remote
       #'make-nearby-temp-file
     #'make-temp-file)
   prefix dir-flag suffix))

(defun apheleia--create-rcs-patch (old-buffer new-buffer remote callback)
  "Generate RCS patch from text in OLD-BUFFER to text in NEW-BUFFER.
Once finished, invoke CALLBACK with a buffer containing the patch
as its sole argument.

See `apheleia--run-formatters' for a description of REMOTE."
  (apheleia--log
   'rcs "Creating RCS patch between buffers with %d and %d bytes"
   (buffer-size old-buffer) (buffer-size new-buffer))
  ;; Make sure at least one of the two buffers is saved to a file. The
  ;; other one we can feed on stdin.
  (let ((old-fname
         (with-current-buffer old-buffer
           (and (not (buffer-modified-p)) buffer-file-name)))
        (new-fname
         (with-current-buffer new-buffer
           (and (not (buffer-modified-p)) buffer-file-name)))
        ;; Place any temporary files we must delete in here.
        (clear-files nil)
        (run-on-remote (and (eq apheleia-remote-algorithm 'remote)
                            remote)))
    (cl-labels ((;; Weird indentation because of differences in Emacs
                 ;; indentation algorithm between 27 and 28
                 apheleia--make-temp-file-for-rcs-patch
                 (buffer &optional fname)
                 ;; Ensure there's a file with the contents of `buffer' on the
                 ;; target machine. `fname', if given, refers to an existing
                 ;; file that may not exist on the target machine and needs
                 ;; to be copied over.
                 (let ((fname-remote (and fname (file-remote-p fname))))
                   (when (or (not fname)
                             (not (equal run-on-remote fname-remote)))
                     (setq fname
                           (apheleia--make-temp-file run-on-remote "apheleia"))
                     (push fname clear-files)
                     (with-current-buffer buffer
                       (apheleia--write-region-silently
                        (point-min) (point-max) fname)))
                   (apheleia-formatters-local-buffer-file-name fname))))
      ;; Ensure file is on target right machine, or create a copy of it.
      (when old-fname
        (setq old-fname
              (apheleia--make-temp-file-for-rcs-patch old-buffer old-fname)))
      (when new-fname
        (setq new-fname
              (apheleia--make-temp-file-for-rcs-patch new-buffer new-fname)))
      ;; When neither files have an open file-handle, create one.
      (unless (or old-fname new-fname)
        (setq new-fname (apheleia--make-temp-file-for-rcs-patch new-buffer))))

    (let ((ctx (apheleia-formatter--context)))
      (setf (apheleia-formatter--name ctx) nil ; Skip logging on failure
            (apheleia-formatter--arg1 ctx) "diff"
            (apheleia-formatter--argv ctx) `("--rcs" "--strip-trailing-cr" "--"
                                             ,(or old-fname "-")
                                             ,(or new-fname "-"))
            (apheleia-formatter--remote ctx) remote
            (apheleia-formatter--stdin ctx)
            (if new-fname old-buffer new-buffer))

      (apheleia--execute-formatter-process
       :ctx ctx
       :callback callback
       :ensure
       (lambda ()
         (dolist (file clear-files)
           (ignore-errors
             (delete-file file))))
       ;; Exit status is 0 if no changes, 1 if some changes, and 2 if
       ;; error.
       :exit-status (lambda (status) (memq status '(0 1)))))))

(defun apheleia--safe-buffer-name ()
  "Return `buffer-name' without special file-system characters."
  ;; See https://stackoverflow.com/q/1976007 for a list of supported
  ;; characters on all systems.
  (replace-regexp-in-string
   (rx (or "/" "<" ">" ":" "\"" "\\" "|" "?" "*"))
   ""
   (buffer-name)))

(defun apheleia--replq (dest in out)
  "Replace all references to IN with OUT in DEST.
This function does not modify DEST in place, it returns a copy."
  (setq in (apheleia--ensure-list in))
  (mapcar (lambda (arg)
            (if (memq arg in)
                out
              arg))
          dest))

(defun apheleia--formatter-context (name command remote &optional stdin-buffer)
  "Construct a formatter context for the formatter with NAME and COMMAND.
Returns an `apheleia-formatter--context' object on success and nil if
the formatter is not executable. The returned formatter context may
have some state such as temporary files that the caller is expected
to cleanup.

STDIN-BUFFER is the optional buffer to use when creating a temporary
file for the formatters standard input. REMOTE asserts whether the
buffer being formatted is on a remote machine or the local machine.
See `apheleia--run-formatters' for more details on the usage of REMOTE.

If COMMAND uses the symbol `file' and the current buffer is modified
from what is written to disk, then return nil meaning meaning no
cmd is to be run."
  (cl-block nil
    (let* ((context (apheleia-formatter--context))
           (run-on-remote
            (when (eq apheleia-remote-algorithm 'remote)
              remote))
           ;; Whether the machine the process will run on matches
           ;; the machine the buffer/file is currently on. Either
           ;; we're running remotely and the buffer is remote or
           ;; we're not running remotely and the buffer is not
           ;; remote.
           (remote-match (equal run-on-remote remote))
           (stdin (or stdin-buffer (current-buffer)))
           (command (apply #'list command)))
      (setf (apheleia-formatter--name context) name)
      (setf (apheleia-formatter--stdin context) stdin)
      (setf (apheleia-formatter--remote context) remote)
      ;; TODO: Support arbitrary package managers, not just NPM.
      (when (memq 'npx command)
        (setq command (remq 'npx command))
        (when remote-match
          (when-let ((project-dir
                      (locate-dominating-file default-directory
                                              "node_modules")))
            (let ((binary
                   (expand-file-name
                    (car command)
                    (expand-file-name
                     ".bin"
                     (expand-file-name "node_modules" project-dir)))))
              (when (file-executable-p binary)
                (setcar command binary))))))

      (when (or (memq 'file command) (memq 'filepath command))
        ;; Fail when using file but not as the first formatter in this
        ;; sequence. (But filepath is okay, since it indicates content
        ;; is not actually being read from the named file.)
        (when (memq 'file command)
          (when stdin-buffer
            (error "Cannot run formatter using `file' in a sequence unless \
it's first in the sequence"))
          (unless remote-match
            (error "Formatter uses `file' but process will run on different \
machine from the machine file is available on"))
          (setq stdin nil)
          ;; If `buffer-file-name' is nil then there is no backing
          ;; file, so `buffer-modified-p' should be ignored (it always
          ;; returns non-nil).
          (when (and (buffer-modified-p) buffer-file-name)
            (cl-return)))
        ;; We always strip out the remote-path prefix for file/filepath.
        (let ((file-name (apheleia-formatters-local-buffer-file-name
                          (or buffer-file-name
                              (concat default-directory
                                      (apheleia--safe-buffer-name))))))
          (setq command (apheleia--replq command '(file filepath) file-name))))

      (when (or (memq 'input command) (memq 'inplace command))
        (let ((input-fname (apheleia--make-temp-file
                            run-on-remote "apheleia" nil
                            (when-let ((file-name
                                        (or buffer-file-name
                                            (apheleia--safe-buffer-name))))
                              (file-name-extension file-name 'period)))))
          (with-current-buffer stdin
            (apheleia--write-region-silently nil nil input-fname))
          (setf (apheleia-formatter--input-fname context) input-fname
                (apheleia-formatter--stdin context) nil)
          ;; Inplace is the same as input but the output file is the
          ;; input file.
          (when (memq 'inplace command)
            (setf (apheleia-formatter--output-fname context) input-fname))
          (setq command (apheleia--replq
                         command '(input inplace)
                         (apheleia-formatters-local-buffer-file-name
                          input-fname)))))

      (when (memq 'output command)
        (let ((output-fname (apheleia--make-temp-file
                             run-on-remote "apheleia")))
          (setf (apheleia-formatter--output-fname context) output-fname)
          (setq command (apheleia--replq
                         command 'output
                         (apheleia-formatters-local-buffer-file-name
                          output-fname)))))

      ;; Evaluate each element of arg that isn't a string and replace
      ;; it with the evaluated value. The result of an evaluation should
      ;; be a string or a list of strings. If the former its replaced as
      ;; is. If the latter the contents of the list is substituted in
      ;; place.
      (setq command
            (cl-loop
             for arg in command
             with val = nil
             do (setq val (if (stringp arg)
                              arg
                            (eval arg)))
             if val
             if (and (consp val)
                     (cl-every #'stringp val))
             append val
             else if (stringp val)
             collect val
             else do (error "Result of command evaluation must be a string \
or list of strings: %S" arg)))

      ;; Windows fails to run formatter scripts. Check whether the
      ;; command executable is a script that contains a shebang. Parse
      ;; the shebang and insert the binary into the command.
      (when (member system-type '(ms-dos windows-nt))
        (when-let ((arg1-file (locate-file (car command) exec-path)))
          (with-temp-buffer
            (insert-file-contents arg1-file nil 0 2)
            (when (string= (buffer-string) "#!")
              ;; Assumes that the full shebang is max 200 characters
              (insert-file-contents arg1-file nil 2 200 t)
              (let* ((shebang-components (split-string (thing-at-point 'line)))
                     (shebang-binary
                      (if (string= (car shebang-components) "/usr/bin/env")
                          (cdr shebang-components)
                        (last (split-string (car shebang-components) "/")))))
                (setq command (append shebang-binary
                                      (list arg1-file)
                                      (cdr command))))))))

      (setf (apheleia-formatter--arg1 context) (car command)
            (apheleia-formatter--argv context) (cdr command))
      context)))

(defun apheleia--run-formatter-process
    (command buffer remote callback stdin formatter)
  "Run a formatter using a shell command.
COMMAND should be a list of string or symbols for the formatter that
will format the current buffer. See `apheleia--run-formatters' for a
description of COMMAND, BUFFER, CALLBACK, REMOTE, and STDIN. FORMATTER
is the symbol of the current formatter being run, for diagnostic
purposes."
  ;; NOTE: We switch to the original buffer both to format the command
  ;; correctly and also to ensure any buffer local variables correctly
  ;; resolve for the whole formatting process (for example
  ;; `apheleia--current-process').
  (with-current-buffer buffer
    (when-let* ((script-dir (expand-file-name
                             "scripts/formatters"
                             (file-name-directory
                              (file-truename
                               ;; Borrowed with love from Magit
                               (let ((load-suffixes '(".el")))
                                 (locate-library "apheleia"))))))
                ;; Gotta set both `exec-path' and the PATH env-var,
                ;; the former is for Emacs itself while the latter is
                ;; for subprocesses of the proc we start.
                (exec-path (cons script-dir exec-path))
                (process-environment
                 (cons (concat "PATH=" script-dir ":" (getenv "PATH"))
                       process-environment))
                (ctx
                 (apheleia--formatter-context formatter command remote stdin)))
      (if (executable-find (apheleia-formatter--arg1 ctx)
                           (eq apheleia-remote-algorithm 'remote))
          (apheleia--execute-formatter-process
           :ctx ctx
           :callback
           (lambda (err stdout)
             (if err
                 (funcall callback err stdout)
               (when-let
                   ((output-fname (apheleia-formatter--output-fname ctx)))
                 ;; Load output-fname contents into the stdout buffer.
                 (with-current-buffer stdout
                   (erase-buffer)
                   (insert-file-contents-literally output-fname)))
               (funcall callback nil stdout)))
           :ensure
           (lambda ()
             (dolist (fname (list (apheleia-formatter--input-fname ctx)
                                  (apheleia-formatter--output-fname ctx)))
               (when fname
                 (ignore-errors (delete-file fname))))))
        (let ((errmsg
               (format
                "Could not find executable for formatter %s, skipping"
                formatter)))
          (apheleia--log 'process "%s" errmsg)
          (funcall callback (cons 'error errmsg) nil))))))

(defun apheleia--run-formatter-function
    (func buffer remote callback stdin formatter)
  "Run a formatter using a Lisp function FUNC.
See `apheleia--run-formatters' for a description of BUFFER, REMOTE,
CALLBACK and STDIN. FORMATTER is the symbol of the current formatter
being run, for diagnostic purposes."
  (let* ((formatter-name (if (symbolp func) (symbol-name func) "lambda"))
         (scratch (generate-new-buffer
                   (format " *apheleia-%s-scratch*" formatter-name))))
    (with-current-buffer scratch
      ;; We expect FUNC to modify scratch in place so we can't simply pass
      ;; STDIN to it. When STDIN isn't nil, it's the output of a previous
      ;; formatter and we want to keep it alive so we can debug any issues
      ;; with it.
      (insert-buffer-substring (or stdin buffer))
      (funcall func
               ;; Original buffer being formatted. This shouldn't be
               ;; modified. You can use it to check things like the
               ;; current major mode, or the buffer filename. If you
               ;; use it as input for the formatter, your formatter
               ;; won't work when chained after another formatter.
               :buffer buffer
               ;; Buffer the formatter should modify. This starts out
               ;; containing the original file contents, which will be
               ;; the same as `buffer' except it has already been
               ;; transformed by any formatters that ran previously.
               :scratch scratch
               ;; Name of the current formatter symbol, e.g. `black'.
               :formatter formatter
               ;; Callback. Should pass an error value (cons of symbol
               ;; and data, like for `signal') or nil. For backwards
               ;; compatibility it can also invoke only on success,
               ;; with no args.
               :callback
               (lambda (&optional err)
                 (unwind-protect
                     (funcall callback err (when (not err) scratch))
                   (kill-buffer scratch)))
               ;; The remote part of the buffers file-name or directory.
               :remote remote
               ;; Whether the formatter should be run async or not.
               :async (not remote)
               ;; Callback when formatting scratch has failed.
               :callback
               (apply-partially #'kill-buffer scratch)))))

(cl-defun apheleia-indent-lisp-buffer
    (&key buffer scratch callback &allow-other-keys)
  "Format a Lisp BUFFER.
Use SCRATCH as a temporary buffer and CALLBACK to apply the
transformation.

For more implementation detail, see
`apheleia--run-formatter-function'."
  (with-current-buffer scratch
    (funcall (with-current-buffer buffer major-mode))
    (setq-local indent-line-function
                (buffer-local-value 'indent-line-function buffer))
    (setq-local lisp-indent-function
		(buffer-local-value 'lisp-indent-function buffer))
    (setq-local indent-tabs-mode
                (buffer-local-value 'indent-tabs-mode buffer))
    (goto-char (point-min))
    (let ((inhibit-message t)
          (message-log-max nil))
      (indent-region (point-min) (point-max)))
    (funcall callback)))

(cl-defun apheleia--run-formatters
    (formatters buffer remote callback &optional stdin)
  "Run one or more code formatters on the current buffer.
FORMATTERS is a list of symbols that appear as keys in
`apheleia-formatters'. BUFFER is the `current-buffer' when this
function was first called.

CALLBACK is always invoked unless there is a synchronous nonlocal
exit, the first argument is nil or an error. In the case of no
error, the second argument is a buffer containing the output of
all the formatters, otherwise it is nil.

REMOTE asserts whether the buffer being formatted is on a remote
machine or the current machine. It should be the output of
`file-remote-p' on the current variable `buffer-file-name'.
REMOTE is the remote part of the original buffers file-name or
directory'. It's used alongside `apheleia-remote-algorithm' to
determine where the formatter process and any temporary files it
may need should be placed.

STDIN is a buffer containing the standard input for the first
formatter in COMMANDS. This should not be supplied by the caller
and instead is supplied by this command when invoked recursively.
The stdout of the previous formatter becomes the stdin of the
next formatter."
  (apheleia--log
   'run-formatter
   "Running formatters %S on buffer %S" formatters buffer)
  (let ((command (alist-get (car formatters) apheleia-formatters)))
    (funcall
     (cond
      ((consp command)
       #'apheleia--run-formatter-process)
      ((or (functionp command)
           (symbolp command))
       #'apheleia--run-formatter-function)
      (t
       (error "Formatter must be a shell command or a Lisp \
function: %s" command)))
     command
     buffer
     remote
     (lambda (err stdout)
       (if err
           (funcall callback err stdout)
         (condition-case-unless-debug err
             (unless (string-empty-p
                      (with-current-buffer stdout (buffer-string)))
               (if (cdr formatters)
                   ;; Forward current stdout to remaining formatters,
                   ;; passing along the current callback and using the
                   ;; current formatters output as stdin.
                   (apheleia--run-formatters
                    (cdr formatters) buffer remote callback stdout)
                 (funcall callback nil stdout)))
           (error (funcall callback err nil)))))
     stdin
     (car formatters))))

;;;###autoload
(defvar-local apheleia-formatter nil
  "Name of formatter to use in current buffer, a symbol or nil.
If non-nil, then `apheleia-formatters' should have a matching
entry. This overrides `apheleia-mode-alist'.

The value can also be a list of symbols to apply multiple
formatters in sequence.")

(defun apheleia--formatter-safe-p (val)
  "Return non-nil if VAL is a good value for `apheleia-formatter'."
  (or (symbolp val)
      (and (listp val)
           (cl-every #'symbolp val))))

(put 'apheleia-formatter 'safe-local-variable #'apheleia--formatter-safe-p)

(defun apheleia--ensure-list (arg)
  "Ensure ARG is a list of length at least 1.
When ARG is not a list its turned into a list."
  (if (listp arg)
      arg
    (list arg)))

(defun apheleia--get-mode-chain ()
  "Return list of major modes in current buffer.
This is a list starting with `major-mode' and followed by its
parents, if any."
  (let ((modes (list major-mode)))
    (while (get (car modes) 'derived-mode-parent)
      (push (get (car modes) 'derived-mode-parent) modes))
    (nreverse modes)))

(defun apheleia--get-formatters (&optional interactive)
  "Return the list of formatters to use for the current buffer.
This is a list of symbols that may appear as cars in
`apheleia-formatters', or nil if no formatter is configured for
the current buffer.

Consult the values of `apheleia-mode-alist' and
`apheleia-formatter' to determine which formatter is configured.
Consult also `apheleia-mode-predicates', if non-nil.

If INTERACTIVE is non-nil, then prompt the user for which
formatter to run if none is configured, instead of returning nil.
If INTERACTIVE is the special symbol `prompt', then prompt
even if a formatter is configured."
  (or (and (not (eq interactive 'prompt))
           (apheleia--ensure-list
            (or apheleia-formatter
                ;; Go through the mode alist. There are two types of
                ;; entries, mode and regex. We should return whichever
                ;; entry matches first in the list. However, if two
                ;; modes match, then we should return the entry for
                ;; the more specific mode.
                ;;
                ;; Implementation: Iterate once. If we match a regex,
                ;; immediately return, unless we already matched a
                ;; mode (setting the `formatters' variable), in which
                ;; case do not return, but also keep going to see if
                ;; there is a more specific mode later in the list. If
                ;; we match a mode, save the entry for later
                ;; reference, as well as the mode that matched it.
                ;; Update that saved entry only when we find a more
                ;; specific mode (i.e., a mode that is derived from
                ;; but not equal to the previously saved mode). Return
                ;; at the end of the loop the saved entry, if we
                ;; didn't exit early.
                (let* ((unset (make-symbol "gensym-unset"))
                       (matched-mode nil)
                       (formatters unset)
                       (mode major-mode))
                  (cl-dolist (pred apheleia-mode-predicates)
                    (when-let ((new-mode (funcall pred)))
                      (setq mode new-mode)
                      (cl-return)))
                  (cl-dolist (entry apheleia-mode-alist
                                    (unless (eq formatters unset)
                                      formatters))
                    (when (and (stringp (car entry))
                               buffer-file-name
                               (string-match-p
                                (car entry) buffer-file-name)
                               (eq formatters unset))
                      (cl-return (cdr entry)))
                    (when (and (symbolp (car entry))
                               (provided-mode-derived-p mode (car entry))
                               (or (eq formatters unset)
                                   (and
                                    (not (eq (car entry) matched-mode))
                                    (provided-mode-derived-p
                                     (car entry) matched-mode))))
                      (setq matched-mode (car entry))
                      (setq formatters (cdr entry))))))))
      (and interactive
           (list
            (intern
             (completing-read
              "Formatter: "
              (or (map-keys apheleia-formatters)
                  (user-error
                   "No formatters in `apheleia-formatters'"))
              nil 'require-match))))))

(provide 'apheleia-formatters)

;;; apheleia-formatters.el ends here
