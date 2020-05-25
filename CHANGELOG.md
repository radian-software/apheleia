# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog].

## Unreleased
### Bugs fixed
* Previously, there were some race conditions related to switching the
  current buffer. In particular, if you switched the current buffer
  right after saving, or save two buffers in quick succession, then it
  was possible for one buffer to be overwritten or to not be
  formatted. These problems have been fixed ([#8]).
* Previously, enabling `undo-tree-auto-save-history` caused Apheleia
  to mark the buffer as modified after formatting. This has been
  fixed ([#10]).
* Under some circumstances Apheleia would cause the buffer's undo
  history to be lost. An attempt has been made to fix this, although
  it's not obvious how to reproduce the problem so the fix has not
  been verified ([#15]).

[#8]: https://github.com/raxod502/apheleia/issues/8
[#10]: https://github.com/raxod502/apheleia/issues/10
[#15]: https://github.com/raxod502/apheleia/issues/15

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
