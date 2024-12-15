;;; apheleia-formatter-context.el --- Formatter ctx -*- lexical-binding: t -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file defines a helper for encapsulating common state for a formatter
;; process.

;;; Code:

(require 'eieio)

(defclass apheleia-formatter--context ()
  ((name
    :documentation "The symbol identifier for this formatter.
Set this to nil if the command being run does not correspond to a formatter."
    :accessor apheleia-formatter--name)
   (arg1
    :documentation "Process used to invoke formatter."
    :accessor apheleia-formatter--arg1)
   (argv
    :documentation "Extra command line arguments for the formatter."
    :accessor apheleia-formatter--argv)
   (remote
    :documentation "Whether this formatter should run on a remote machine.
When set apheleia will use the formatter buffers file-handler, allowing the
process to be spawned on remote machines."
    :accessor apheleia-formatter--remote)
   (stdin
    :documentation "Input buffer.
Set to nil when the formatter reads from a file-path instead of standard
input."
    :accessor apheleia-formatter--stdin)
   (input-fname
    :documentation "Optional path to a temporary copy of the input buffer.
When set the stdin slot is not set and the formatter will be reading from this
file path. `apheleia' will delete this file on cleanup."
    :accessor apheleia-formatter--input-fname
    :initform nil)
   (output-fname
    :documentation "Optional path to an temporary output file.
When set the formatter process is meant to write the formatted input to this
file. `apheleia' will delete this file on cleanup."
    :accessor apheleia-formatter--output-fname
    :initform nil)
   (exit-status
    :documentation "The exit-code of the formatter process.
This is unset until after the process is run."
    :accessor apheleia-formatter--exit-status))
  :documentation "Maintain the state of a formatter process.")

(provide 'apheleia-formatter-context)

;;; apheleia-formatter-context.el ends here
