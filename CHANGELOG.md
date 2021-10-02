# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog].

## Unreleased
### Formatters
* [ClangFormat](https://clang.llvm.org/docs/ClangFormat.html) for
  C/C++,
  [`google-java-format`](https://github.com/google/google-java-format)
  for Java, [latexindent](https://ctan.org/pkg/latexindent?lang=en)
  for [LaTeX](https://www.latex-project.org/), and
  [rustfmt](https://github.com/rust-lang/rustfmt) for
  [Rust](https://www.rust-lang.org/) ([#24]).
* [Mix format](https://hexdocs.pm/mix/1.12/Mix.Tasks.Format.html) for Elixir

### Bugs fixed
* Fix spelling error in generated process names ([#32]).
* Apheleia no longer conflicts with undo-fu (#[39]).
* Apheleia no longer triggers `after-set-visited-file-name-hook`,
  which reduces conflicts with various modes. For example, `lsp-mode`
  will no longer trigger a reconnect every time you save.
* Fix inconsistent `:type` spec preventing use of `customize-variable`
  on `apheleia-formatters`.

[#24]: https://github.com/raxod502/apheleia/pull/24
[#30]: https://github.com/raxod502/apheleia/issues/30
[#32]: https://github.com/raxod502/apheleia/pull/32
[#39]: https://github.com/raxod502/apheleia/issues/39

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
