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
  (require 'rx)
  (declare-function bibtex-reformat "bibtex" (&optional beg end)))
(autoload 'bibtex-reformat "bibtex" nil t)

(defcustom apheleia-formatters
  '((astyle . ("astyle" (apheleia-formatters-locate-file
                         "--options" ".astylerc")))
    (asmfmt . ("asmfmt"))
    (bean-format . ("bean-format" input))
    (beautysh . ("beautysh"
                 (apheleia-formatters-indent
                  "--tab" "--indent-size" 'sh-basic-offset)
                 "-"))
    (bibtex-format . apheleia-reformat-bibtex-buffer)
    (black . ("black"
              (when (apheleia-formatters-extension-p "pyi") "--pyi")
              (apheleia-formatters-fill-column "--line-length")
              "--stdin-filename" filepath
              "-"))
    (brittany . ("brittany"))
    (buildifier . ("buildifier" "-type"
                   (cond
                    ((eq major-mode 'bazel-workspace-mode) "workspace")
                    ((eq major-mode 'bazel-module-mode) "module")
                    ((eq major-mode 'bazel-build-mode) "build")
                    (t "auto")
                    )))
    (biome . ("apheleia-npx" "biome" "format" "--stdin-file-path" filepath))
    (caddyfmt . ("caddy" "fmt" "-"))
    (clang-format . ("clang-format"
                     "-assume-filename"
                     (or (apheleia-formatters-local-buffer-file-name)
                         (apheleia-formatters-mode-extension)
                         ".c")))
    (cljfmt . ("cljfmt" "fix" "-"))
    (cljstyle . ("cljstyle" "pipe"))
    (cmake-format . ("cmake-format" "-"))
    (crystal-tool-format . ("crystal" "tool" "format" "-"))
    (csharpier . ("csharpier" "format"))
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
    (fourmolu . ("fourmolu" "--stdin-input-file" filepath))
    (gawk . ("gawk" "-f" "-" "--pretty-print=-"))
    (gdformat . ("gdformat" "-"))
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
    (hurlfmt . ("hurlfmt" "--no-color"))
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
    (nomad . ("nomad" "fmt" "-"))
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
    ;; rest of file unchanged...
