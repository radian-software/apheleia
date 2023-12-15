;;; apheleia.el --- Reformat buffer stably -*- lexical-binding: t -*-

;; Copyright (C) 2019-2022 Radian LLC and contributors

;; Author: Radian LLC <contact+apheleia@radian.codes>
;; Created: 7 Jul 2019
;; Homepage: https://github.com/radian-software/apheleia
;; Keywords: tools
;; Package-Requires: ((emacs "27"))
;; SPDX-License-Identifier: MIT
;; Version: 4.0

;;; Commentary:

;; Apheleia is an Emacs Lisp package which allows you to reformat a
;; buffer without moving point. This solves the usual problem of
;; running a tool like Prettier or Black on `before-save-hook', namely
;; that it resets point to the beginning of the buffer. Apheleia
;; maintains the position of point relative to its surrounding text
;; even if the buffer is modified by the reformatting.

;; Please see https://github.com/radian-software/apheleia for more information.

;;; Code:

(require 'apheleia-formatters)
(require 'apheleia-log)
(require 'apheleia-rcs)

(defgroup apheleia nil
  "Reformat buffer without moving point."
  :group 'external
  :link '(url-link :tag "GitHub" "https://github.com/radian-software/apheleia")
  :link '(emacs-commentary-link :tag "Commentary" "apheleia"))

(defcustom apheleia-mode-lighter " Apheleia"
  "Lighter for `apheleia-mode'."
  :type '(choice :tag "Lighter" (const :tag "No lighter" nil) string)
  :risky t
  :group 'apheleia)

(defun apheleia--buffer-hash ()
  "Compute hash of current buffer."
  (if (fboundp 'buffer-hash)
      (buffer-hash)
    (md5 (current-buffer))))

(defun apheleia--disallowed-p ()
  "Return an error message if Apheleia cannot be run, else nil."
  (when (and buffer-file-name
             (file-remote-p (or buffer-file-name
                                default-directory))
             (eq apheleia-remote-algorithm 'cancel))
    "Apheleia refused to run formatter due to `apheleia-remote-algorithm'"))

;;;###autoload
(defun apheleia-format-buffer (formatter &optional callback)
  "Run code formatter asynchronously on current buffer, preserving point.

FORMATTER is a symbol appearing as a key in
`apheleia-formatters', or a list of them to run multiple
formatters in a chain. If called interactively, run the currently
configured formatters (see `apheleia-formatter' and
`apheleia-mode-alist'), or prompt from `apheleia-formatters' if
there is none configured for the current buffer. With a prefix
argument, prompt always.

After the formatters finish running, the diff utility is invoked to
determine what changes it made. That diff is then used to apply the
formatter's changes to the current buffer without moving point or
changing the scroll position in any window displaying the buffer. If
the buffer has been modified since the formatter started running,
however, the operation is aborted.

If the formatter actually finishes running and the buffer is
successfully updated (even if the formatter has not made any
changes), CALLBACK, if provided, is invoked with no arguments."
  (interactive (progn
                 (when-let ((err (apheleia--disallowed-p)))
                   (user-error err))
                 (list (apheleia--get-formatters
                        (if current-prefix-arg
                            'prompt
                          'interactive)))))
  (apheleia--log
   'format-buffer
   "Invoking apheleia-format-buffer on %S with formatter %S"
   (current-buffer)
   formatter)
  (let ((formatters (apheleia--ensure-list formatter)))
    ;; Check for this error ahead of time so we don't have to deal
    ;; with it anywhere in the internal machinery of Apheleia.
    (dolist (formatter formatters)
      (unless (alist-get formatter apheleia-formatters)
        (user-error
         "No such formatter defined in `apheleia-formatters': %S"
         formatter)))
    ;; Fail silently if disallowed, since we don't want to throw an
    ;; error on `post-command-hook'. We already took care of throwing
    ;; `user-error' on interactive usage above.
    (if-let ((err (apheleia--disallowed-p)))
        (apheleia--log
         'format-buffer
         "Aborting in %S due to apheleia--disallowed-p: %s"
         (buffer-name (current-buffer))
         err)
      ;; It's important to store the saved buffer hash in a lexical
      ;; variable rather than a dynamic (global) one, else multiple
      ;; concurrent invocations of `apheleia-format-buffer' can
      ;; overwrite each other, and get the wrong results about whether
      ;; the buffer was actually modified since the formatting
      ;; operation started, leading to data loss.
      ;;
      ;; https://github.com/radian-software/apheleia/issues/226
      (let ((saved-buffer-hash (apheleia--buffer-hash)))
        (let ((cur-buffer (current-buffer))
              (remote (file-remote-p (or buffer-file-name
                                         default-directory))))
          (apheleia--run-formatters
           formatters
           cur-buffer
           remote
           (lambda (formatted-buffer)
             (if (not (buffer-live-p cur-buffer))
                 (apheleia--log
                  'format-buffer
                  "Aborting in %S because buffer has died"
                  (buffer-name cur-buffer))
               (with-current-buffer cur-buffer
                 ;; Short-circuit.
                 (if (not (equal saved-buffer-hash (apheleia--buffer-hash)))
                     (apheleia--log
                      'format-buffer
                      "Aborting in %S because contents have changed"
                      (buffer-name cur-buffer))
                   (apheleia--create-rcs-patch
                    cur-buffer formatted-buffer remote
                    (lambda (patch-buffer)
                      (when (buffer-live-p cur-buffer)
                        (with-current-buffer cur-buffer
                          (if (not (equal
                                    saved-buffer-hash
                                    (apheleia--buffer-hash)))
                              (apheleia--log
                               'format-buffer
                               "Aborting in %S because contents have changed"
                               (buffer-name cur-buffer))
                            (apheleia--apply-rcs-patch
                             (current-buffer) patch-buffer)
                            (if (not callback)
                                (apheleia--log
                                 'format-buffer
                                 (concat
                                  "Skipping callback because "
                                  "none was provided"))
                              (apheleia--log
                               'format-buffer "Invoking callback")
                              (funcall callback)))))))))))))))))

(defcustom apheleia-post-format-hook nil
  "Normal hook run after Apheleia formats a buffer successfully."
  :type 'hook
  :group 'apheleia)

(defcustom apheleia-inhibit-functions nil
  "List of functions that prevent Apheleia from turning on automatically.
If one of these returns non-nil then `apheleia-mode' is not
enabled in a buffer, even if `apheleia-global-mode' is on. You
can still manually enable `apheleia-mode' in such a buffer.

See also `apheleia-inhibit' for another way to accomplish a
similar task."
  :type '(repeat function)
  :group 'apheleia)

;; Handle recursive references.
(defvar apheleia-mode)

;; Prevent infinite loop.
(defvar apheleia-format-after-save-in-progress nil
  "Prevent `apheleia-format-after-save' from being called recursively.
This will be locally bound to t while `apheleia-format-after-save' is
operating, to prevent an infinite loop.")

;; Autoload because the user may enable `apheleia-mode' without
;; loading Apheleia; thus this function may be invoked as an autoload.
;;;###autoload
(defun apheleia-format-after-save ()
  "Run code formatter for current buffer if any configured, then save."
  (unless apheleia-format-after-save-in-progress
    (when (and apheleia-mode (not (buffer-narrowed-p)))
      (when-let ((formatters (apheleia--get-formatters)))
        (apheleia-format-buffer
         formatters
         (lambda ()
           (with-demoted-errors "Apheleia: %s"
             (when buffer-file-name
               (let ((apheleia-format-after-save-in-progress t))
                 (apheleia--save-buffer-silently)))
             (run-hooks 'apheleia-post-format-hook))))))))

;; Use `progn' to force the entire minor mode definition to be copied
;; into the autoloads file, so that the minor mode can be enabled
;; without pulling in all of Apheleia during init.
;;;###autoload
(progn

  (define-minor-mode apheleia-mode
    "Minor mode for reformatting code on save without moving point.
It is customized by means of the variables `apheleia-mode-alist'
and `apheleia-formatters'."
    :lighter apheleia-mode-lighter
    (if apheleia-mode
        (add-hook 'after-save-hook #'apheleia-format-after-save nil 'local)
      (remove-hook 'after-save-hook #'apheleia-format-after-save 'local)))

  (defvar-local apheleia-inhibit nil
    "Do not enable `apheleia-mode' automatically if non-nil.
This is designed for use in .dir-locals.el.

See also `apheleia-inhibit-functions'.")
  (put 'apheleia-inhibit 'safe-local-variable #'booleanp)

  (defun apheleia-mode-maybe ()
    "Enable `apheleia-mode' if allowed by user configuration.
This checks `apheleia-inhibit-functions' and `apheleia-inhibit'
to see if it is allowed."
    (unless (or
             apheleia-inhibit
             (run-hook-with-args-until-success
              'apheleia-inhibit-functions))
      (apheleia-mode)))

  (define-globalized-minor-mode apheleia-global-mode
    apheleia-mode apheleia-mode-maybe
    :group 'apheleia)

  (put 'apheleia-mode 'safe-local-variable #'booleanp))

(provide 'apheleia)

;;; apheleia.el ends here
