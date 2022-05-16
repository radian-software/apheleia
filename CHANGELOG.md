# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog].

## Unreleased
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
* [ktlint](https://github.com/pinterest/ktlint) for Kotlin ([#95]).
* [nixfmt](https://github.com/serokell/nixfmt) for Nix ([#98]).

### Features
* Support remote files and buffers that were opened through TRAMP
  ([#33]).

[#33]: https://github.com/raxod502/apheleia/issues/33
[#87]: https://github.com/raxod502/apheleia/pull/87
[#89]: https://github.com/raxod502/apheleia/pull/89
[#94]: https://github.com/radian-software/apheleia/pull/94
[#95]: https://github.com/radian-software/apheleia/pull/95
[#98]: https://github.com/radian-software/apheleia/pull/98

## 2.0
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

[#23]: https://github.com/raxod502/apheleia/issues/23
[#47]: https://github.com/raxod502/apheleia/issues/47
[#52]: https://github.com/raxod502/apheleia/issues/52
[#58]: https://github.com/raxod502/apheleia/issues/58
[#60]: https://github.com/raxod502/apheleia/issues/60
[#62]: https://github.com/raxod502/apheleia/issues/62
[#64]: https://github.com/raxod502/apheleia/issues/64
[#65]: https://github.com/raxod502/apheleia/pull/65
[#68]: https://github.com/raxod502/apheleia/issues/68
[#69]: https://github.com/raxod502/apheleia/issues/69
[#74]: https://github.com/raxod502/apheleia/pull/74

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

[#24]: https://github.com/raxod502/apheleia/pull/24
[#30]: https://github.com/raxod502/apheleia/issues/30
[#31]: https://github.com/raxod502/apheleia/issues/31
[#32]: https://github.com/raxod502/apheleia/pull/32
[#39]: https://github.com/raxod502/apheleia/issues/39
[#48]: https://github.com/raxod502/apheleia/pull/48
[#49]: https://github.com/raxod502/apheleia/pull/49
[#50]: https://github.com/raxod502/apheleia/pull/50
[#51]: https://github.com/raxod502/apheleia/pull/51
[#54]: https://github.com/raxod502/apheleia/pull/54
[#55]: https://github.com/raxod502/apheleia/issues/55
[#64]: https://github.com/raxod502/apheleia/issues/64
[#65]: https://github.com/raxod502/apheleia/pull/65

## 1.1.2 (released 2021-02-26)
### Enhancements
* Prettier is now enabled in `json-mode`.

### Bugs fixed
* Prettier now respects `.prettierignore` ([#21]).
* Apheleia's global mode should no longer trigger warnings about a locally
  let-bound `after-save-hook` ([#27]).

[#21]: https://github.com/raxod502/apheleia/issues/21
[#27]: https://github.com/raxod502/apheleia/issues/27

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

[#8]: https://github.com/raxod502/apheleia/issues/8
[#10]: https://github.com/raxod502/apheleia/issues/10
[#12]: https://github.com/raxod502/apheleia/pull/12
[#19]: https://github.com/raxod502/apheleia/pull/19

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

[#4]: https://github.com/raxod502/apheleia/issues/4
[#9]: https://github.com/raxod502/apheleia/pull/9

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
