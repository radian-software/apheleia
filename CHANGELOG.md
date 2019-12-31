# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog].

## Unreleased
### Bugs fixed
* Previously, weirdness could happen if manually running Prettier via
  `M-x apheleia-format-buffer` on a buffer which was modified from
  what was written to disk. Now we simply abort running a command that
  uses the `file` keyword if the buffer is modified, since it will not
  produce correct results. This should not affect normal usage of
  Apheleia.

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
