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
   patch](https://web.archive.org/web/20220527003730/https://tools.ietf.org/doc/tcllib/html/rcs.html#section4)
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

## Installation

Apheleia is available on [MELPA](https://melpa.org/). It is easiest
to install it using
[`straight.el`](https://github.com/radian-software/straight.el):

    (straight-use-package 'apheleia)

However, you may install using any other package manager if you
prefer.

### Dependencies

Emacs 27 or later is supported. Apheleia does not include any
formatters. You must install any formatter separately that you wish to
use. As long as it is on `$PATH` then Apheleia will pick it up
automatically; missing formatters will silently be skipped, but errors
from invoking installed formatters will be reported on buffer save.

It is recommended to have Bash installed, as this is used as a
dependency for Apheleia to invoke certain formatters (e.g.
Node.js-based formatters).

Windows support is not guaranteed due to lack of support for common
open standards on this platform. Pull requests adjusting Apheleia for
improved cross-platform portability will be accepted, but no
guarantees are made about stability on Windows.

## User guide

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
    * You can manipulate this alist using standard Emacs functions.
      For example, to add some command-line options to Black, you
      could use:

      ```elisp
      (setf (alist-get 'black apheleia-formatters)
            '("black" "--option" "..." "-"))
      ```
    * There are a list of symbols that are interpreted by apheleia
      specially when formatting a command (example: `npx`). Any
      non-string entries in a formatter that doesn't equal one of
      these symbols is evaluated and replaced in place. This can be
      used to pass certain flags to the formatter process depending on
      the state of the current buffer. For example:

      ```elisp
      (push '(shfmt . ("beautysh"
                       "-filename" filepath
                       (when-let ((indent (bound-and-true-p sh-basic-offset)))
                         (list "--indent-size" (number-to-string indent)))
                       (when indent-tabs-mode "--tab")
                       "-"))
            apheleia-formatters)
      ```

      This adds an entry to `apheleia-formatters` for the `beautysh`
      formatter. The evaluated entries makes it so that the `--tab`
      flag is only passed to `beautysh` when the value of
      `indent-tabs-mode` is true. Similarly the indent-size flag is
      passed the exact value of the `sh-basic-offset` variable
      only when it is bound. Observe that one of these evaluations
      returns a list of flags whereas the other returns a single
      string. These are substituted into the command as you'd expect.
    * You can also use Apheleia to format buffers that have no underlying
      files. In this case the value of `file` and `filepath` will be
      the name of the current buffer with any special characters for
      the file-system (such as `*` on windows) being stripped out.

      This is also how the extension for any temporary files apheleia
      might create will be determined. If you're using a formatter
      that determines the file-type from the extension you should name
      such buffers such that their suffixed with the extension. For
      example a buffer called `*foo-bar.c*` that has no associated
      file will have an implicit file-name of `foo-bar.c` and any
      temporary files will be suffixed with a `.c` extension.
    * You can implement formatters as arbitrary Elisp functions which
      operate directly on a buffer, without needing to invoke an
      external command. This can be useful to integrate with e.g.
      language servers. See the docstring for more information on the
      expected interface for Elisp formatters.
* `apheleia-mode-alist`: Alist mapping major modes and filename
  regexps to names of formatters to use in those modes and files. See
  the docstring for more information.
    * You can use this variable to configure multiple formatters for
      the same buffer by setting the `cdr` of an entry to a list of
      formatters to run instead of a single formatter. For example you
      may want to run `isort` and `black` one after the other.

      ```elisp
      (setf (alist-get 'isort apheleia-formatters)
            '("isort" "--stdout" "-"))
      (setf (alist-get 'python-mode apheleia-mode-alist)
            '(isort black))
      ```

      This will make apheleia run `isort` on the current buffer and then
      `black` on the result of `isort` and then use the final output to
      format the current buffer.

      **Warning**: At the moment there's no smart or configurable
      error handling in place. This means if one of the configured
      formatters fail (for example if `isort` isn't installed) then
      apheleia just doesn't format the buffer at all, even if `black`
      is installed.

      **Warning:** If a formatter uses `file` (rather than `filepath`
      or `input` or none of these keywords), it can't be chained after
      another formatter, because `file` implies that the formatter
      must read from the *original* file, not an intermediate
      temporary file. For this reason it's suggested to avoid the use
      of `file` in general.
* `apheleia-formatter`: Optional buffer-local variable specifying the
  formatter to use in this buffer. Overrides `apheleia-mode-alist`.
  You can set this in a local variables list, or in `.dir-locals.el`
  (e.g. `((python-mode . ((apheleia-formatter . (isort black)))))`),
  or in a custom hook of your own that sets the local variable
  conditionally.
* `apheleia-inhibit`: Optional buffer-local variable, if set to
  non-nil then Apheleia does not turn on automatically even if
  `apheleia-global-mode` is on.

You can run `M-x apheleia-mode` to toggle automatic formatting on save
in a single buffer, or `M-x apheleia-global-mode` to toggle the
default setting for all buffers. Also, even if `apheleia-mode` is not
enabled, you can run `M-x apheleia-format-buffer` to manually invoke
the configured formatter for the current buffer. Running with a prefix
argument will cause the command to prompt you for which formatter to
run.

Apheleia does not currently support TRAMP, and is therefore
automatically disabled for remote files.

If an error occurs while formatting, a message is displayed in the
echo area. You can jump to the error by invoking `M-x
apheleia-goto-error`, or manually switch to the log buffer mentioned
in the message.

You can configure error reporting using the following user options:

* `apheleia-hide-log-buffers`: By default, errors from formatters are
  put in buffers named like `*apheleia-cmdname-log*`. If you customize
  this user option to non-nil then a space is prepended to the names
  of these buffers, hiding them by default in `switch-to-buffer` (you
  must type a space to see them).
* `apheleia-log-only-errors`: By default, only failed formatter runs
  are logged. If you customize this user option to nil then all runs
  are logged, along with whether or not they succeeded. This could be
  helpful in debugging.

The following user options are also available:

* `apheleia-post-format-hook`: Normal hook run after Apheleia formats
  a buffer. Run if the formatting is successful, even when no changes
  are made to the buffer.
* `apheleia-max-alignment-size`: The maximum number of characters that
  a diff region can have to be processed using Apheleia's dynamic
  programming algorithm for point alignment. This cannot be too big or
  Emacs will hang noticeably on large reformatting operations, since
  the DP algorithm is quadratic-time.
* `apheleia-mode-lighter`: `apheleia-mode` lighter displayed in the
  mode-line. If you don't want to display it, use nil. Otherwise, its
  value must be a string.

Apheleia exposes some hooks for advanced customization:

* `apheleia-formatter-exited-hook`: Abnormal hook which is run after a
  formatter has completely finished running for a buffer. Not run if
  the formatting was interrupted and no action was taken. Receives two
  arguments: the symbol for the formatter that was run (e.g. `black`,
  or it could be a list if multiple formatters were run in a chain),
  and a boolean for whether there was an error.

* `apheleia-inhibit-functions`: List of functions to run before
  turning on Apheleia automatically from `apheleia-global-mode`. If
  one of these returns non-nil then `apheleia-mode` is not enabled in
  the buffer.

* `apheleia-skip-functions`: List of functions to run before *each*
  Apheleia formatter invocation. If one of these returns non-nil then
  the formatter is not run, even if `apheleia-mode` is enabled.

### Formatter configuration

There is no configuration interface in Apheleia for formatter
behavior. The way to configure a formatter is by editing a standard
config file that it reads (e.g. `.prettierrc.json`), or setting an
environment variable that it reads, or by changing the entry in
`apheleia-formatters` to customize the command-line arguments.

There is one exception to this, which is that Apheleia's default
command-line arguments for the built-in formatters will automatically
check Emacs' indentation options for the corresponding major mode, and
pass that information to the formatter. This way, the indentation
(tabs vs spaces, and how many) applied by the formatter will match
what electric indentation in Emacs is doing, preventing a shuffle back
and forth as you type.

This behavior can be disabled by setting
`apheleia-formatters-respect-indent-level` to nil.

## Troubleshooting

Try running your formatter outside of Emacs to verify it works there.
Check what command-line options it is configured with in
`apheleia-formatters`.

To debug internal bugs, race conditions, or performance issues, try
setting `apheleia-log-debug-info` to non-nil and check the contents of
`*apheleia-debug-log*`. It will have detailed trace information about
most operations performed by Apheleia.

### Known issues

* `process aphelieia-whatever no longer connected to pipe; closed it`:
  This happens on older Emacs versions when formatting a buffer with
  size greater than 65,536 characters. There is no known workaround
  besides disabling `apheleia-mode` for the affected buffer, or
  upgrading to a more recent version of Emacs. See
  [#20](https://github.com/radian-software/apheleia/issues/20).

## Contributing

Please see [the contributor guide for my
projects](https://github.com/radian-software/contributor-guide) for
general information, and the following sections for Apheleia-specific
details.

There's also a [wiki](https://github.com/radian-software/apheleia/wiki) that could do with additions/clarity. Any
improvement suggestions should be submitted as an issue.

### Adding a formatter

I have done my best to make it straightforward to add a formatter. You
just follow these steps:

1. Install your formatter on your machine so you can test.
2. Create an entry in `apheleia-formatters` with how to run it. (See
   the docstring of this variable for explanation about the available
   keywords.)
3. Add entries for the relevant major modes in `apheleia-mode-alist`.
4. See if it works for you!
5. Add a file at `test/formatters/installers/yourformatter.bash` which
   explains how to install the formatter on Ubuntu. This will be used
   by CI.
6. Test with `make fmt-build FORMATTERS=yourformatter` to do the
   installation, then `make fmt-docker` to start a shell with the
   formatter available. Verify it runs in this environment.
7. Add an example input (pre-formatting) and output (post-formatting)
   file at `test/formatters/samplecode/yourformatter/in.whatever` and
   `test/formatters/samplecode/yourformatter/out.whatever`.
8. Verify that the tests are passing, using `make fmt-test
   FORMATTERS=yourformatter` from inside the `fmt-docker` shell.
9. Submit a pull request, CI should now be passing!

## Acknowledgements

I got the idea for using RCS patches to avoid moving point too much
from [prettier-js](https://github.com/prettier/prettier-emacs),
although that package does not implement the dynamic programming
algorithm which Apheleia uses to guarantee stability of point even
within a formatted region.

Note that despite this inspiration, Apheleia is a clean-room
implementation which is free of the copyright terms of prettier-js.
