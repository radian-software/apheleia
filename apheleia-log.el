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

;;;###autoload
(defun apheleia-goto-error ()
  "Go to the most recently reported formatter error message."
  (interactive)
  (unless apheleia--last-error-marker
    (user-error "No error has happened yet"))
  (pop-to-buffer (marker-buffer apheleia--last-error-marker))
  (goto-char apheleia--last-error-marker))

(defun apheliea-log--buffer-name (formatter)
  "Get the name of the log buffer for FORMATTER."
  (format "%s*apheleia-%s-log*"
          (if apheleia-hide-log-buffers
              " "
            "")
          formatter))

(defun apheleia-log--formatter-result
    (ctx log-buffer exit-ok directory stderr-string)
  "Log the result of a formatter process.
CTX The formatter process context (see `apheleia-formatter--context').
LOG-BUFFER is the name of the log-buffer.
EXIT-OK is true when the formatter exited sucesfully.
DIRECTORY is the directory in which the formatter ran.
STDERR-STRING is the stderr output of the formatter."
  (with-current-buffer (get-buffer-create log-buffer)
    (special-mode)
    (save-restriction
      (widen)
      (let ((inhibit-read-only t)
            (orig-point (point))
            (keep-at-end (eobp)))
        (goto-char (point-max))
        (skip-chars-backward "\n")
        (delete-region (point) (point-max))
        (unless (bobp)
          (insert "\n\n\C-l\n"))

        (unless exit-ok
          (unless apheleia--last-error-marker
            (setq apheleia--last-error-marker (make-marker)))
          (move-marker apheleia--last-error-marker (point)))

        (insert
         (current-time-string)
         " :: "
         directory
         "\n$ "
         (mapconcat #'shell-quote-argument
                    `(,(apheleia-formatter--arg1 ctx)
                      ,@(apheleia-formatter--argv ctx))
                    " ")
         "\n\n"
         (if (string-empty-p stderr-string)
             "(no output on stderr)"
           stderr-string)
         "\n\n"
         "Command "
         (if exit-ok "succeeded" "failed")
         " with exit code "
         (number-to-string (apheleia-formatter--exit-status ctx))
         ".\n")
        ;; Known issue: this does not actually work; point is left at the end
        ;; of the previous command output, instead of being moved to the end of
        ;; the buffer for some reason.
        (goto-char
         (if keep-at-end
             (point-max)
           (min (point-max) orig-point)))
        (goto-char (point-max))))))

(provide 'apheleia-log)

;;; apheleia-log.el ends here
