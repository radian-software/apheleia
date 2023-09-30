;;; apheleia-log.el --- Log utilities -*- lexical-binding: t -*-

;;; Commentary:

;; Helpers for `apheleia' logging.

;;; Code:

(require 'subr-x)

(require 'apheleia-formatter-context)

(defcustom apheleia-hide-log-buffers nil
  "Non-nil means log buffers will be hidden.
Hidden buffers have names that begin with a space, and do not
appear in `switch-to-buffer' unless you type in a space
manually."
  :type 'boolean
  :group 'apheleia)

(defcustom apheleia-log-only-errors t
  "Non-nil means Apheleia will only log when an error occurs.
Otherwise, Apheleia will log every time a formatter is run, even
if it is successful."
  :type 'boolean
  :group 'apheleia)

(defvar apheleia--last-error-marker nil
  "Marker for the last error message for any formatter.
This points into a log buffer.")

(defun apheleia-goto-error ()
  "Go to the most recently reported formatter error message."
  (interactive)
  (unless apheleia--last-error-marker
    (user-error "No error has happened yet"))
  (pop-to-buffer (marker-buffer apheleia--last-error-marker))
  (goto-char apheleia--last-error-marker))

(provide 'apheleia-log)

;;; apheleia-log.el ends here
