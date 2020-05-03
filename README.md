# Apheleia

Good code is automatically formatted by tools like
[Black](https://github.com/python/black) or
[Prettier](https://prettier.io/) so that you and your team spend less
time on formatting and more time on building features. It's best if
your editor can run code formatters each time you save a file, so that
you don't have to look at badly formatted code or get surprised when
things change just before you commit. However, running a code
formatter on save suffers from the following two problems:

1. It takes some time (e.g. around 200ms for Black on an empty file),
   which makes the editor feel less responsive.
2. It invariably moves your cursor (point) somewhere unexpected if the
   changes made by the code formatter are too close to point's
   position.

Apheleia is an [Emacs](https://www.gnu.org/software/emacs/) package
which solves both of these problems comprehensively for all languages,
allowing you to say goodbye to language-specific packages such as
[Blacken](https://github.com/proofit404/blacken) and
[prettier-js](https://github.com/prettier/prettier-emacs).

The approach is as follows:

1. Run code formatters on `after-save-hook`, rather than
   `before-save-hook`, and do so asynchronously. Once the formatter
   has finished running, check if the buffer has been modified since
   it started; only apply the changes if not.
2. After running the code formatter, generate an [RCS
   patch](https://tools.ietf.org/doc/tcllib/html/rcs.html#section4)
   showing the changes and then apply it to the buffer. This prevents
   changes elsewhere in the buffer from moving point. If a patch
   region happens to include point, then use a [dynamic programming
   algorithm for string
   alignment](https://en.wikipedia.org/wiki/Needleman%E2%80%93Wunsch_algorithm)
   to determine where point should be moved so that it remains in the
   same place relative to its surroundings. Finally, if the vertical
   position of point relative to the window has changed, adjust the
   scroll position to maintain maximum visual continuity. (This
   includes iterating through all windows displaying the buffer, if
   there are more than one.) The dynamic programming algorithm runs in
   quadratic time, which is why it is only applied if necessary and to
   a single patch region.

## User guide

To get started with Apheleia, install it with
[`straight.el`](https://github.com/raxod502/straight.el) as follows:

    (straight-use-package '(apheleia :host github :repo "raxod502/apheleia"))

Alternatively, you can use another source-based package manager such
as [Quelpa](https://framagit.org/steckerhalter/quelpa),
[El-Get](https://github.com/dimitri/el-get), or
[Borg](https://github.com/emacscollective/borg). Apheleia is not
currently listed on [MELPA](https://melpa.org/) or [GNU
ELPA](https://elpa.gnu.org/).

To your init-file, add the following form:

    (apheleia-global-mode +1)

The autoloading has been configured so that this will *not* cause
Apheleia to be loaded until you save a file.

By default, Apheleia is configured to format with
[Black](https://github.com/python/black),
[Prettier](https://prettier.io/), and
[Gofmt](https://golang.org/cmd/gofmt/) on save in all relevant major
modes. To configure this, you can adjust the values of the following
variables:

* `apheleia-formatters`: Alist mapping names of formatters (symbols
  like `black` and `prettier`) to commands used to run those
  formatters (such as `("black" "-")` and `(npx "prettier" input)`).
  See the docstring for more information.
* `apheleia-mode-alist`: Alist mapping major modes and filename
  regexps to names of formatters to use in those modes and files. See
  the docstring for more information.
* `apheleia-formatter`: Optional buffer-local variable specifying the
  formatter to use in this buffer. Overrides `apheleia-mode-alist`.

You can run `M-x apheleia-mode` to toggle automatic formatting on save
in a single buffer, or `M-x apheleia-global-mode` to toggle the
default setting for all buffers. Also, even if `apheleia-mode` is not
enabled, you can run `M-x apheleia-format-buffer` to manually invoke
the configured formatter for the current buffer. Running with a prefix
argument will cause the command to prompt you for which formatter to
run.

Apheleia does not currently support TRAMP, and is therefore
automatically disabled for remote files.

The following user options are also available:

* `apheleia-post-format-hook`: Normal hook run after Apheleia formats
  a buffer. Run if the formatting is successful, even when no changes
  are made to the buffer.
* `apheleia-max-alignment-size`: The maximum number of characters that
  a diff region can have to be processed using Apheleia's dynamic
  programming algorithm for point alignment. This cannot be too big or
  Emacs will hang noticeably on large reformatting operations, since
  the DP algorithm is quadratic-time.

## Contributing

Please see [the contributor guide for my
projects](https://github.com/raxod502/contributor-guide).

## Acknowledgements

I got the idea for using RCS patches to avoid moving point too much
from [prettier-js](https://github.com/prettier/prettier-emacs),
although that package does not implement the dynamic programming
algorithm which Apheleia uses to guarantee stability of point even
within a formatted region.

Note that despite this inspiration, Apheleia is a clean-room
implementation which is free of the copyright terms of prettier-js.
