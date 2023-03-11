# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog].

## Unreleased
### Bugs fixed
* `ktlint` would emit log messages into its stdout when formatting,
  and these would get spliced into the source file. This has been fixed
  by suppressing all logs from `ktlint`.

## 3.2 (released 2023-02-25)
### Features
* You can use `apheleia-inhibit` as a file-local variable to disable
  Apheleia turning on automatically for a file or directory. You can
  also use `apheleia-inhibit-functions` to configure custom logic to
  prevent Apheleia from turning on automatically under certain
  circumstances, without needing to adjust file-local variables. See
  [#134] and [#138].
* `apheleia-mode` lighter is now customizable ([#143]).

### Changes
* shfmt is no longer enabled by default in order to prevent corruption
  of zsh scripts. Re-enabling shfmt in a future release is intended,
  but depends on further implementation work in Apheleia to only
  enable it for supported shells.

### Enhancements
* Emacs 29's tree-sitter-based major modes have been added to
  `apheleia-mode-alist` ([#145], [#148], [#151], [#155]).

### Bugs fixed
* If a formatter exited with a zero exit code when being killed by a
  signal (even though that is really weird behavior and arguably a bug
  in the formatter), then if a file was saved multiple times in quick
  succession, its contents could be erased and replaced with an error
  message from the formatter. This has been fixed. See [#131] for more
  details.
* Npx commands in apheleia-formatters are not overwritten when
  expanding binary path to node_modules directory ([#152]).
* Error is no longer reported when the first element of a command is
  an evaluable form rather than a literal string ([#156]).

### Formatters
* [`crystal tool
  format`](https://crystal-lang.org/reference/1.6/man/crystal/index.html#crystal-tool-format)
  for [Crystal](https://crystal-lang.org/) ([#137]).
* [gofumpt](https://pkg.go.dev/mvdan.cc/gofumpt) and
  [goimports](https://pkg.go.dev/golang.org/x/tools/cmd/goimports) for
  [Go](https://golang.google.cn/) ([#147]).
* [`caddy fmt`](https://caddyserver.com/docs/command-line#caddy-fmt)
  for [Caddyfile](https://caddyserver.com/docs/caddyfile), used by
  [Caddy 2](https://caddyserver.com/) ([#136])

[#131]: https://github.com/radian-software/apheleia/issues/131
[#134]: https://github.com/radian-software/apheleia/issues/134
[#136]: https://github.com/radian-software/apheleia/issues/136
[#137]: https://github.com/radian-software/apheleia/pull/137
[#138]: https://github.com/radian-software/apheleia/pull/138
[#143]: https://github.com/radian-software/apheleia/pull/143
[#145]: https://github.com/radian-software/apheleia/pull/145
[#147]: https://github.com/radian-software/apheleia/pull/147
[#148]: https://github.com/radian-software/apheleia/pull/148
[#152]: https://github.com/radian-software/apheleia/pull/152

## 3.1 (released 2022-11-11)
### Enhancements
* shfmt uses 4 spaces instead of tabs by default.
* Formatters using `'filepath` (OCamlFormat and Prettier) are no
  longer prevented from running on a modified buffer ([#109], [#110]).
* Buffer content is now always passed to formatters using a pipe. This
  fixes issues with formatters that behave differently when receiving
  input on stdin versus being run on a tty ([#119]).
* Prettier now specifies `--parser` argument explicitly, so it will
  work properly even when the name of the file does not match what
  Prettier expects (e.g. `.yamllint` will be formatted as YAML by
  Prettier as long as it is in `yaml-mode`). See [#103].

### Bugs fixed
* When a formatter has a bug and fails to return anything on stdout
  (e.g. scalafmt), do not erase the buffer ([#116]).
* Fix `Error: "Selecting deleted buffer"` which occurred in some
  circumstances due to a race condition ([#123]).
* Apheleia does not delete the contents of narrowed buffers when
  running. Instead, it is disabled in narrowed buffers. Support for
  narrowed buffers may be added in future but it has never been
  correctly supported in the past (see [#43]). More at [#124], [#127].
* Currently, when a formatter invoked via a process isn't installed
  correctly; it throws an error. It doesn't make sense to attempt to
  format if we can't find the correct formatter, so instead formatting
  is only attempted when the formatter is found. [#126]
* clang-format doesn't handle filenames correctly by default. Support
  for guessing the flags from first the file name then the major mode
  in the case of a temporary buffer was added in [#128]
* Fix handling of formatters using `inplace` in some circumstances
  ([#132]).

### Formatters
* [elm-format](https://github.com/avh4/elm-format) for Elm ([#100]).
* [bean-format](https://github.com/beancount/beancount) for Beancount
  ([#101]).
* [stylua](https://github.com/JohnnyMorganz/StyLua) for Lua ([#105]).
* Native Emacs indentation of Emacs Lisp code as a formatter ([#102]).
  This is not enabled by default but you can enable it by adding
  `(emacs-lisp-mode . lisp-indent)` to `apheleia-mode-alist`

### Bugfixes
* Prettier supports SCSS instead of SASS. The original support for
  SASS in Apheleia was a bug because Prettier actually never had
  support for SASS in the first place, so Apheleia would have failed
  anyway on trying to format a SASS file.

[#43]: https://github.com/radian-software/apheleia/issues/43
[#100]: https://github.com/radian-software/apheleia/pull/100
[#101]: https://github.com/radian-software/apheleia/pull/101
[#102]: https://github.com/radian-software/apheleia/pull/102
[#103]: https://github.com/radian-software/apheleia/issues/103
[#105]: https://github.com/radian-software/apheleia/pull/105
[#109]: https://github.com/radian-software/apheleia/issues/109
[#110]: https://github.com/radian-software/apheleia/pull/110
[#116]: https://github.com/radian-software/apheleia/pull/116
[#119]: https://github.com/radian-software/apheleia/pull/119
[#123]: https://github.com/radian-software/apheleia/issues/123
[#124]: https://github.com/radian-software/apheleia/issues/124
[#125]: https://github.com/radian-software/apheleia/pull/125
[#126]: https://github.com/radian-software/apheleia/pull/126
[#127]: https://github.com/radian-software/apheleia/pull/127
[#128]: https://github.com/radian-software/apheleia/pull/128
[#132]: https://github.com/radian-software/apheleia/pull/132

## 3.0 (released 2022-06-01)
### Breaking changes
* The arguments of formatters defined as Elisp functions has changed.
  A formatter function should now be a `cl-defun` taking key-value
  arguments with support for more keys being provided on a later
  apheleia release (by including `&allow-other-keys` in the parameter
  list). For a list of provided arguments see
  `apheleia--run-formatter-function`.
* Emacs 25 is no longer supported.

### Enhancements
* Ocamlformat is now used in `caml-mode` in addition to `tuareg-mode`
  ([#94]).

### Formatters
* [dart-format](https://dart.dev/tools/dart-format) for Dart ([#89]).
* [phpcs](https://github.com/squizlabs/PHP_CodeSniffer) for PHP
  ([#87]).
* [ktlint](https://github.com/pinterest/ktlint) for Kotlin ([#97]).
* [nixfmt](https://github.com/serokell/nixfmt) for Nix ([#98]).

### Features
* Support remote files and buffers that were opened through TRAMP
  ([#33]).

[#33]: https://github.com/radian-software/apheleia/issues/33
[#87]: https://github.com/radian-software/apheleia/pull/87
[#89]: https://github.com/radian-software/apheleia/pull/89
[#94]: https://github.com/radian-software/apheleia/pull/94
[#97]: https://github.com/radian-software/apheleia/pull/97
[#98]: https://github.com/radian-software/apheleia/pull/98

## 2.0 (released 2022-04-10)
### Breaking changes
* The interface to `apheleia-format-buffer` has changed. You now pass
  in the symbol of a formatter from `apheleia-formatters` (or a list
  of them) rather than the actual command. This change improves the
  ability of Apheleia to report useful error messages and logging.
* Stdout and stderr buffers are no longer retained after running a
  formatter. Instead, the stderr is appended into an
  `*apheleia-cmdname-log*` buffer if it fails, or unconditionally if
  the new user option `apheleia-log-only-errors` is set to nil. See
  [#64], [#65]. The log buffer is not hidden by default, unlike the
  old stdout and stderr buffers, but this can be changed with the new
  user option `apheleia-hide-log-buffers`. Also, the log shows all
  command output rather than just the latest run. You can add further
  customizations using the new hook `apheleia-formatter-exited-hook`
  ([#69]).

### Features
* Apheleia can now format buffers that do not have an underlying file
  ([#52]).
* You can now use a Lisp function as a formatter, by providing a
  symbol or lambda in `apheleia-formatters` rather than a list of
  strings ([#62]). The function should be a `cl-defun` taking key-value
  arguments with support for more keys being provided on a later
  apheleia release (by including `&allow-other-keys` in the parameter
  list). For a list of provided arguments see
  `apheleia--run-formatter-function`.
* Formatters that operate on files in place are now supported, by
  using the symbol `inplace` in an entry on `apheleia-formatters` to
  stand in for the name of a temporary file that will be modified in
  place by the formatter ([#23]).

### Enhancements
* The buffer-local variable `apheleia-formatter` is now marked as safe
  ([#74]). This allows you to configure the formatter that Apheleia
  will use in a file-local variable. Note: only formatters already
  declared in `apheleia-formatters` can be used; this does not allow
  arbitrary shell commands to be specified in file-local variables.

### Bugs fixed
* Allow running the same formatter in multiple buffers in parallel
  ([#64], [#65]). Previously, when saving a number of files at the
  same time, the contents of those buffers could be corrupted by a
  race condition.
* In some circumstances the error `wrong-type-argument bufferp nil`
  could be reported when running certain formatters under Apheleia.
  This has been fixed.
* Rustfmt is no longer passed the `--unstable-features` and
  `--skip-children` flags, since they are not available on all
  versions of Rustfmt ([#69]).
* When a formatter cannot be found, orphaned stderr processes are no
  longer created and left around forever ([#47]).
* Apheleia no longer resets the major mode to its default value when
  formatting a buffer. This was never intended and the bug has now
  been fixed ([#58]).

### Formatters added
* [fish\_indent](https://fishshell.com/docs/current/cmds/fish_indent.html)
  for [Fish](https://fishshell.com/) ([#68]).

### Formatter bugs fixed
* isort is passed different arguments so as not to trigger a crash
  that appears to be related to the `--stdout` flag.
* latexindent now has its log file disabled so as to avoid cluttering
  the working directory.
* ocamlformat is now configured to work even outside of detected
  projects.

[#23]: https://github.com/radian-software/apheleia/issues/23
[#47]: https://github.com/radian-software/apheleia/issues/47
[#52]: https://github.com/radian-software/apheleia/issues/52
[#58]: https://github.com/radian-software/apheleia/issues/58
[#60]: https://github.com/radian-software/apheleia/issues/60
[#62]: https://github.com/radian-software/apheleia/issues/62
[#64]: https://github.com/radian-software/apheleia/issues/64
[#65]: https://github.com/radian-software/apheleia/pull/65
[#68]: https://github.com/radian-software/apheleia/issues/68
[#69]: https://github.com/radian-software/apheleia/issues/69
[#74]: https://github.com/radian-software/apheleia/pull/74

## 1.2 (released 2021-12-27)
### Enhancements
* Support multiple formatters ([#31]). You can now configure a list of
  formatters for a major-mode in `apheleia-mode-alist` and they will
  be run in sequence.
* Support evaluating items in `apheleia-formatters` to make formatter
  commands more dynamic ([#50], [#55]).

### Formatters
* [ClangFormat](https://clang.llvm.org/docs/ClangFormat.html) for
  C/C++
* [`google-java-format`](https://github.com/google/google-java-format)
  for Java
* [isort](https://github.com/PyCQA/isort) for
  [Python](https://www.python.org/) ([#51])
* [latexindent](https://ctan.org/pkg/latexindent?lang=en) for
  [LaTeX](https://www.latex-project.org/)
* [rustfmt](https://github.com/rust-lang/rustfmt) for
  [Rust](https://www.rust-lang.org/) ([#24])
* [`mix format`](https://hexdocs.pm/mix/1.12/Mix.Tasks.Format.html)
  for Elixir ([#48])
* [`@prettier/plugin-ruby`](https://github.com/prettier/plugin-ruby)
  for Ruby ([#49])

### Bugs fixed
* Fix spelling error in generated process names ([#32]).
* Apheleia no longer conflicts with undo-fu ([#39]).
* Apheleia no longer triggers `after-set-visited-file-name-hook`,
  which reduces conflicts with various modes. For example, `lsp-mode`
  will no longer trigger a reconnect every time you save.
* Fix inconsistent `:type` spec preventing use of `customize-variable`
  on `apheleia-formatters`.
* Fix mixed style line ending generated by `diff` ([#54]) by adding
  `--strip-trailing-cr` to `diff`'s argument list.

[#24]: https://github.com/radian-software/apheleia/pull/24
[#30]: https://github.com/radian-software/apheleia/issues/30
[#31]: https://github.com/radian-software/apheleia/issues/31
[#32]: https://github.com/radian-software/apheleia/pull/32
[#39]: https://github.com/radian-software/apheleia/issues/39
[#48]: https://github.com/radian-software/apheleia/pull/48
[#49]: https://github.com/radian-software/apheleia/pull/49
[#50]: https://github.com/radian-software/apheleia/pull/50
[#51]: https://github.com/radian-software/apheleia/pull/51
[#54]: https://github.com/radian-software/apheleia/pull/54
[#55]: https://github.com/radian-software/apheleia/issues/55
[#64]: https://github.com/radian-software/apheleia/issues/64
[#65]: https://github.com/radian-software/apheleia/pull/65

## 1.1.2 (released 2021-02-26)
### Enhancements
* Prettier is now enabled in `json-mode`.

### Bugs fixed
* Prettier now respects `.prettierignore` ([#21]).
* Apheleia's global mode should no longer trigger warnings about a locally
  let-bound `after-save-hook` ([#27]).

[#21]: https://github.com/radian-software/apheleia/issues/21
[#27]: https://github.com/radian-software/apheleia/issues/27

## 1.1.1 (released 2020-07-16)
### Formatters
* New: [OCamlFormat](https://github.com/ocaml-ppx/ocamlformat) for
  [Ocaml](https://ocaml.org/) ([#19]).

### Bugs fixed
* Previously, there were some race conditions related to switching the
  current buffer. In particular, if you switched the current buffer
  right after saving, or save two buffers in quick succession, then it
  was possible for one buffer to be overwritten or to not be
  formatted. These problems have been fixed ([#8]).
* Previously, enabling `undo-tree-auto-save-history` caused Apheleia
  to mark the buffer as modified after formatting. This has been
  fixed ([#10]).
* Apheleia now tries harder to accidentally enter an infinite
  formatting loop ([#12]).

[#8]: https://github.com/radian-software/apheleia/issues/8
[#10]: https://github.com/radian-software/apheleia/issues/10
[#12]: https://github.com/radian-software/apheleia/pull/12
[#19]: https://github.com/radian-software/apheleia/pull/19

## 1.1 (released 2020-04-02)
### Enhancements
* There is now a maximum length for the dynamic programming algorithm,
  controlled by `apheleia-max-alignment-size`. This prevents Emacs
  from hanging due to Apheleia. under any circumstances ([#4]).

### Formatters
* New: [Brittany](https://hackage.haskell.org/package/brittany) for
  [Haskell](https://www.haskell.org/).

### Bugs fixed
* Previously, weirdness could happen if manually running Prettier via
  `M-x apheleia-format-buffer` on a buffer which was modified from
  what was written to disk. Now we simply abort running a command that
  uses the `file` keyword if the buffer is modified, since it will not
  produce correct results. This should not affect normal usage of
  Apheleia.
* Previously, the buffer could be erased when running Apheleia from a
  buffer that had no backing file. This has been fixed, and Apheleia
  can run correctly on buffers both with and without backing files
  ([#9]).

[#4]: https://github.com/radian-software/apheleia/issues/4
[#9]: https://github.com/radian-software/apheleia/pull/9

## 1.0 (released 2019-09-20)
### Added
* Package `apheleia`
* Command `apheleia-format-buffer`
* User options:
  * `apheleia-formatters`, with supported formatters:
    * `black`
    * `prettier`
    * `gofmt`
    * `terraform`
  * `apheleia-mode-alist`
  * `apheleia-post-format-hook`
* Variable `apheleia-formatter`
* Minor mode `apheleia-mode` (and `apheleia-global-mode`)

[keep a changelog]: https://keepachangelog.com/en/1.0.0/
