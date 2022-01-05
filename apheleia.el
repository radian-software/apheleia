;;; apheleia.el --- Reformat buffer stably -*- lexical-binding: t -*-

;; Copyright (C) 2019 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 7 Jul 2019
;; Homepage: https://github.com/raxod502/apheleia
;; Keywords: tools
;; Package-Requires: ((emacs "25.2"))
;; SPDX-License-Identifier: MIT
;; Version: 1.2

;;; Commentary:

;; Apheleia is an Emacs Lisp package which allows you to reformat a
;; buffer without moving point. This solves the usual problem of
;; running a tool like Prettier or Black on `before-save-hook', namely
;; that it resets point to the beginning of the buffer. Apheleia
;; maintains the position of point relative to its surrounding text
;; even if the buffer is modified by the reformatting.

;; Please see https://github.com/raxod502/apheleia for more information.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'subr-x)

(eval-when-compile
  (require 'rx))

(defgroup apheleia nil
  "Reformat buffer without moving point."
  :group 'external
  :link '(url-link :tag "GitHub" "https://github.com/raxod502/apheleia")
  :link '(emacs-commentary-link :tag "Commentary" "apheleia"))

(defcustom apheleia-hide-log-buffers nil
  "Non-nil means log buffers will be hidden.
Hidden buffers have names that begin with a space, and do not
appear in `switch-to-buffer' unless you type in a space
manually."
  :type 'boolean)

(defcustom apheleia-log-only-errors t
  "Non-nil means Apheleia will only log when an error occurs.
Otherwise, Apheleia will log every time a formatter is run, even
if it is successful."
  :type 'boolean)

(defcustom apheleia-formatter-exited-hook nil
  "Abnormal hook run after a formatter has finished running.
Must accept arbitrary keyword arguments. The following arguments
are defined at present:

`:formatter' - The symbol for the formatter that was run.

`:error' - Non-nil if the formatter failed, nil if it succeeded.

`:log' - The log buffer for that formatter, or nil if there is
none (e.g., because logging is not enabled).

This hook is run before `apheleia-after-format-hook', and may be
run multiple times if `apheleia-mode-alist' configures multiple
formatters to run in a chain, with one run per formatter."
  :type 'hook)

(cl-defun apheleia--edit-distance-table (s1 s2)
  "Align strings S1 and S2 for minimum edit distance.
Return the dynamic programming table as has table which maps cons
of integers (I1 . I2) to the edit distance between the first I1
characters of S1 and the first I2 characters of S2."
  (let ((table (make-hash-table :test #'equal)))
    (dotimes (i1 (1+ (length s1)))
      (puthash (cons i1 0) i1 table))
    (dotimes (i2 (1+ (length s2)))
      (puthash (cons 0 i2) i2 table))
    (dotimes (i1 (length s1))
      ;; Iterate from 1 to length+1.
      (cl-incf i1)
      (dotimes (i2 (length s2))
        (cl-incf i2)
        (let ((ins (1+ (gethash (cons i1 (1- i2)) table)))
              (del (1+ (gethash (cons (1- i1) i2) table)))
              (sub (gethash (cons (1- i1) (1- i2)) table)))
          (unless (= (aref s1 (1- i1)) (aref s2 (1- i2)))
            (cl-incf sub))
          (puthash (cons i1 i2) (min ins del sub) table))))
    table))

(defun apheleia--align-point (s1 s2 p1)
  "Given strings S1 and S2 and index P1 in S1, return matching index P2 in S2.
If S1 and S2 are the same, then P1 and P2 will also be the same.
Otherwise, the text of S2 surrounding P2 is \"similar\" to the
text of S1 surrounding P1."
  (let* ((table (apheleia--edit-distance-table s1 s2))
         (i1 (length s1))
         (i2 (length s2)))
    (while (> i1 p1)
      (let ((ins (1+ (gethash (cons i1 (1- i2)) table)))
            (del (1+ (gethash (cons (1- i1) i2) table)))
            (sub (gethash (cons (1- i1) (1- i2)) table)))
        (unless (= (aref s1 (1- i1)) (aref s2 (1- i2)))
          (cl-incf sub))
        (let ((cost (min ins del sub)))
          (cond
           ((= cost ins)
            (cl-decf i2))
           ((= cost del)
            (cl-decf i1))
           ((= cost sub)
            (cl-decf i1)
            (cl-decf i2))))))
    i2))

(defun apheleia--map-rcs-patch (func)
  "Map over the RCS patch in the current buffer.
For each RCS patch command, FUNC is called with an alist that has
the following keys:

- `command': either `addition' or `deletion'
- `start': line number, an integer
- `lines': number of lines to be inserted or removed
- `text': the string to be inserted, only for `addition'

See <https://tools.ietf.org/doc/tcllib/html/rcs.html#section4>
for documentation on the RCS patch format."
  (save-excursion
    (goto-char (point-min))
    (while (not (= (point) (point-max)))
      (unless (looking-at "$\\|\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
        (error "Malformed RCS patch: %S" (point)))
      (forward-line)
      (when-let ((command (match-string 1)))
        (let ((start (string-to-number (match-string 2)))
              (lines (string-to-number (match-string 3))))
          (pcase command
            ("a"
             (let ((text-start (point)))
               (forward-line lines)
               (funcall
                func
                `((command . addition)
                  (start . ,start)
                  (lines . ,lines)
                  (text . ,(buffer-substring-no-properties
                            text-start (point)))))))
            ("d"
             (funcall
              func
              `((command . deletion)
                (start . ,start)
                (lines . ,lines))))))))))

(defcustom apheleia-max-alignment-size 400
  "Maximum size for diff regions that will have point aligned.
Apheleia uses a dynamic programming algorithm to determine where
point should be placed within a diff region, but this algorithm
has quadratic runtime so it will lock up Emacs if it is run on a
diff region that is too large. The value of this variable serves
as a limit on the input size to the algorithm; larger diff
regions will still be applied, but Apheleia won't try to move
point correctly."
  :type 'integer)

(defun apheleia--apply-rcs-patch (content-buffer patch-buffer)
  "Apply RCS patch.
CONTENT-BUFFER contains the text to be patched, and PATCH-BUFFER
contains the patch."
  (let ((commands nil)
        (point-list nil)
        (window-line-list nil))
    (with-current-buffer content-buffer
      (push (cons nil (point)) point-list)
      (dolist (w (get-buffer-window-list nil nil t))
        (push (cons w (window-point w)) point-list)
        (push (cons w (count-lines (window-start w) (point)))
              window-line-list)))
    (with-current-buffer patch-buffer
      (apheleia--map-rcs-patch
       (lambda (command)
         (with-current-buffer content-buffer
           ;; Could be optimized significantly by moving only as many
           ;; lines as needed, rather than returning to the beginning
           ;; of the buffer first.
           (save-excursion
             (goto-char (point-min))
             (forward-line (1- (alist-get 'start command)))
             ;; Account for the off-by-one error in the RCS patch spec
             ;; (namely, text is added *after* the line mentioned in
             ;; the patch).
             (when (eq (alist-get 'command command) 'addition)
               (forward-line))
             (push `(marker . ,(point-marker)) command)
             (push command commands)
             ;; If we delete a region just before inserting new text
             ;; at the same place, then it is a replacement. In this
             ;; case, check if the replaced region includes the window
             ;; point for any window currently displaying the content
             ;; buffer. If so, figure out where that window point
             ;; should be moved to, and record the information in an
             ;; additional command.
             ;;
             ;; See <https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Point.html>.
             ;;
             ;; Note that the commands get pushed in reverse order
             ;; because of how linked lists work.
             (let ((deletion (nth 1 commands))
                   (addition (nth 0 commands)))
               (when (and (eq (alist-get 'command deletion) 'deletion)
                          (eq (alist-get 'command addition) 'addition)
                          ;; Again with the weird off-by-one
                          ;; computations. For example, if you replace
                          ;; lines 68 through 71 inclusive, then the
                          ;; deletion is for line 68 and the addition
                          ;; is for line 70. Blame RCS.
                          (= (+ (alist-get 'start deletion)
                                (alist-get 'lines deletion)
                                -1)
                             (alist-get 'start addition)))
                 (let ((text-start (alist-get 'marker deletion)))
                   (forward-line (alist-get 'lines deletion))
                   (let ((text-end (point)))
                     (dolist (entry point-list)
                       ;; Check if the (window) point is within the
                       ;; replaced region.
                       (cl-destructuring-bind (w . p) entry
                         (when (and (< text-start p)
                                    (< p text-end))
                           (let* ((old-text (buffer-substring-no-properties
                                             text-start text-end))
                                  (new-text (alist-get 'text addition))
                                  (old-relative-point (- p text-start))
                                  (new-relative-point
                                   (if (> (max (length old-text)
                                               (length new-text))
                                          apheleia-max-alignment-size)
                                       old-relative-point
                                     (apheleia--align-point
                                      old-text new-text old-relative-point))))
                             (goto-char text-start)
                             (push `((marker . ,(point-marker))
                                     (command . set-point)
                                     (window . ,w)
                                     (relative-point . ,new-relative-point))
                                   commands))))))))))))))
    (with-current-buffer content-buffer
      (let ((move-to nil))
        (save-excursion
          (dolist (command (nreverse commands))
            (goto-char (alist-get 'marker command))
            (pcase (alist-get 'command command)
              (`addition
               (insert (alist-get 'text command)))
              (`deletion
               (let ((text-start (point)))
                 (forward-line (alist-get 'lines command))
                 (delete-region text-start (point))))
              (`set-point
               (let ((new-point
                      (+ (point) (alist-get 'relative-point command))))
                 (if-let ((w (alist-get 'window command)))
                     (set-window-point w new-point)
                   (setq move-to new-point)))))))
        (when move-to
          (goto-char move-to))))
    ;; Restore the scroll position of each window displaying the
    ;; buffer.
    (dolist (entry window-line-list)
      (cl-destructuring-bind (w . old-window-line) entry
        (let ((new-window-line
               (count-lines (window-start w) (point))))
          (with-selected-window w
            ;; Sometimes if the text is less than a buffer long, and
            ;; we do a deletion, it might not be possible to keep the
            ;; vertical position of point the same by scrolling.
            ;; That's okay. We just go as far as we can.
            (ignore-errors
              (scroll-down (- old-window-line new-window-line)))))))))

(defvar-local apheleia--current-process nil
  "Current process that Apheleia is running, or nil.
Keeping track of this helps avoid running more than one process
at once.")

(defvar apheleia--last-error-marker nil
  "Marker for the last error message for any formatter.
This points into a log buffer.")

(cl-defun apheleia--make-process
    (&key command stdin callback ensure exit-status formatter)
  "Wrapper for `make-process' that behaves a bit more nicely.
COMMAND is as in `make-process'. STDIN, if given, is a buffer
whose contents are fed to the process on stdin. CALLBACK is
invoked with one argument, the buffer containing the text from
stdout, when the process terminates (if it succeeds). ENSURE is a
callback that's invoked whether the process exited sucessfully or
not. EXIT-STATUS is a function which is called with the exit
status of the command; it should return non-nil to indicate that
the command succeeded. If EXIT-STATUS is omitted, then the
command succeeds provided that its exit status is 0. FORMATTER is
the symbol of the formatter that is being run, for diagnostic
purposes. FORMATTER is nil if the command being run does not
correspond to a formatter."
  (when (process-live-p apheleia--current-process)
    (message "Interrupting %s" apheleia--current-process)
    (interrupt-process apheleia--current-process)
    (accept-process-output apheleia--current-process 0.1 nil 'just-this-one)
    (when (process-live-p apheleia--current-process)
      (kill-process apheleia--current-process)))
  (let* ((name (file-name-nondirectory (car command)))
         (stdout (generate-new-buffer
                  (format " *apheleia-%s-stdout*" name)))
         (stderr (generate-new-buffer
                  (format " *apheleia-%s-stderr*" name)))
         (log-name (format "%s*apheleia-%s-log*"
                           (if apheleia-hide-log-buffers
                               " "
                             "")
                           name)))
    (condition-case-unless-debug e
        (progn
          (setq apheleia--current-process
                (make-process
                 :name (format "apheleia-%s" name)
                 :buffer stdout
                 :stderr stderr
                 :command command
                 :noquery t
                 :sentinel
                 (lambda (proc _event)
                   (unless (process-live-p proc)
                     (let ((exit-ok (funcall
                                     (or exit-status #'zerop)
                                     (process-exit-status proc))))
                       ;; Append standard-error from current formatter
                       ;; to log buffer when
                       ;; `apheleia-log-only-errors' is nil or the
                       ;; formatter failed. Every process output is
                       ;; delimited by a line-feed character.
                       (unless (and exit-ok apheleia-log-only-errors)
                         (with-current-buffer (get-buffer-create log-name)
                           (special-mode)
                           (save-restriction
                             (widen)
                             (let ((inhibit-read-only t)
                                   (orig-point (point))
                                   (keep-at-end (eobp))
                                   (stderr-string
                                    (with-current-buffer stderr
                                      (string-trim (buffer-string)))))
                               (goto-char (point-max))
                               (skip-chars-backward "\n")
                               (delete-region (point) (point-max))
                               (unless (bobp)
                                 (insert
                                  "\n\n\C-l\n"))
                               (unless exit-ok
                                 (unless apheleia--last-error-marker
                                   (setq apheleia--last-error-marker
                                         (make-marker))
                                   (move-marker
                                    apheleia--last-error-marker (point))))
                               (insert
                                (current-time-string)
                                " :: "
                                (buffer-local-value 'default-directory stdout)
                                "\n$ "
                                (mapconcat #'shell-quote-argument command " ")
                                "\n\n"
                                (if (string-empty-p stderr-string)
                                    "(no output on stderr)"
                                  stderr-string)
                                "\n\n"
                                "Command "
                                (if exit-ok "succeeded" "failed")
                                " with exit code "
                                (number-to-string (process-exit-status proc))
                                ".\n")
                               ;; Known issue: this does not actually
                               ;; work; point is left at the end of
                               ;; the previous command output, instead
                               ;; of being moved to the end of the
                               ;; buffer for some reason.
                               (goto-char
                                (if keep-at-end
                                    (point-max)
                                  (min
                                   (point-max)
                                   orig-point)))
                               (goto-char (point-max))))))
                       (when formatter
                         (run-hook-with-args
                          'apheleia-formatter-exited-hook
                          :formatter formatter
                          :error (not exit-ok)
                          :log (get-buffer log-name)))
                       (unwind-protect
                           (if exit-ok
                               (when callback
                                 (funcall callback stdout))
                             (message
                              (concat
                               "Failed to run %s: exit status %s "
                               "(see %s %s)")
                              (car command)
                              (process-exit-status proc)
                              (if (string-prefix-p " " log-name)
                                  "hidden buffer"
                                "buffer")
                              (string-trim log-name)))
                         (when ensure
                           (funcall ensure))
                         (ignore-errors
                           (kill-buffer stdout))
                         (ignore-errors
                           (kill-buffer stderr))))))))
          (set-process-sentinel (get-buffer-process stderr) #'ignore)
          (when stdin
            (set-process-coding-system
             apheleia--current-process
             nil
             (buffer-local-value 'buffer-file-coding-system stdin))
            (process-send-string
             apheleia--current-process
             (with-current-buffer stdin
               (buffer-string))))
          (process-send-eof apheleia--current-process))
      (error
       (ignore-errors
         (kill-buffer stdout))
       (ignore-errors
         (kill-buffer stderr))
       (message "Failed to run %s: %s" name (error-message-string e))))))

(defun apheleia-goto-error ()
  "Go to the most recently reported formatter error message."
  (interactive)
  (unless apheleia--last-error-marker
    (user-error "No error has happened yet"))
  (pop-to-buffer (marker-buffer apheleia--last-error-marker))
  (goto-char apheleia--last-error-marker))

(defun apheleia--write-region-silently
    (start end filename &optional
           append visit lockname mustbenew write-region)
  "Like `write-region', but silent.
START, END, FILENAME, APPEND, VISIT, LOCKNAME, and MUSTBENEW are
as in `write-region'. WRITE-REGION is used instead of the actual
`write-region' function, if provided."
  (funcall (or write-region #'write-region)
           start end filename append 0 lockname mustbenew)
  (when (or (eq visit t) (stringp visit))
    (setq buffer-file-name (if (eq visit t)
                               filename
                             visit))
    (set-visited-file-modtime)
    (set-buffer-modified-p nil)))

(defun apheleia--write-file-silently (&optional filename)
  "Write contents of current buffer into file FILENAME, silently.
FILENAME defaults to value of variable `buffer-file-name'. Do not
mark the buffer as visiting FILENAME."
  (cl-letf* ((write-region (symbol-function #'write-region))
             ((symbol-function #'write-region)
              (lambda (start end filename &optional
                             append visit lockname mustbenew)
                (apheleia--write-region-silently
                 start end filename append visit
                 lockname mustbenew write-region)))
             (message (symbol-function #'message))
             ((symbol-function #'message)
              (lambda (format &rest args)
                (unless (equal format "Saving file %s...")
                  (apply message format args))))
             ;; Avoid triggering `after-set-visited-file-name-hook',
             ;; which can have various undesired effects in particular
             ;; major modes. Unfortunately, `write-file' triggers this
             ;; hook unconditionally even if the filename was not
             ;; changed, hence this hack :/
             (run-hooks (symbol-function #'run-hooks))
             ((symbol-function #'run-hooks)
              (lambda (&rest args)
                (unless (equal args '(after-set-visited-file-name-hook))
                  (apply run-hooks args)))))
    (write-file (or filename buffer-file-name))))

(defun apheleia--create-rcs-patch (old-buffer new-buffer callback)
  "Generate RCS patch from text in OLD-BUFFER to text in NEW-BUFFER.
Once finished, invoke CALLBACK with a buffer containing the patch
as its sole argument."
  ;; Make sure at least one of the two buffers is saved to a file. The
  ;; other one we can feed on stdin.
  (let ((old-fname
         (with-current-buffer old-buffer
           (and (not (buffer-modified-p)) buffer-file-name)))
        (new-fname
         (with-current-buffer new-buffer
           (and (not (buffer-modified-p)) buffer-file-name))))
    (unless (or old-fname new-fname)
      (with-current-buffer new-buffer
        (setq new-fname (make-temp-file "apheleia"))
        (apheleia--write-region-silently (point-min) (point-max) new-fname)))
    (apheleia--make-process
     :command `("diff" "--rcs" "--strip-trailing-cr" "--"
                ,(or old-fname "-")
                ,(or new-fname "-"))
     :stdin (if new-fname old-buffer new-buffer)
     :callback callback
     :exit-status (lambda (status)
                    ;; Exit status is 0 if no changes, 1 if some
                    ;; changes, and 2 if error.
                    (memq status '(0 1))))))

(defun apheleia--safe-buffer-name ()
  "Return `buffer-name' without special file-system characters."
  ;; See https://stackoverflow.com/q/1976007 for a list of supported
  ;; characters on all systems.
  (replace-regexp-in-string
   (rx (or "/" "<" ">" ":" "\"" "\\" "|" "?" "*"))
   ""
   (buffer-name)))

(defun apheleia--format-command (command &optional stdin-buffer)
  "Format COMMAND into a shell command and list of file paths.
Returns a list with the car being the optional input file-name, the
cadr being the optional output file-name, the caddr is the buffer to
send as stdin to the formatter (when the input-fname is not used),
and the cdddr being the cmd to run.

STDIN-BUFFER is the optional buffer to use when creating a temporary
file for the formatters standard input.

If COMMAND uses the symbol `file' and the current buffer is modified
from what is written to disk, then return nil meaning meaning no
cmd is to be run."
  (cl-block nil
    (let ((input-fname nil)
          (output-fname nil)
          (stdin (or stdin-buffer (current-buffer)))
          (npx nil))
      ;; TODO: Support arbitrary package managers, not just NPM.
      (when (memq 'npx command)
        (setq npx t)
        (setq command (remq 'npx command)))
      (unless (stringp (car command))
        (error "Command cannot start with %S" (car command)))
      (when npx
        (when-let ((project-dir
                    (locate-dominating-file
                     default-directory "node_modules")))
          (let ((binary
                 (expand-file-name
                  (car command)
                  (expand-file-name
                   ".bin"
                   (expand-file-name
                    "node_modules"
                    project-dir)))))
            (when (file-executable-p binary)
              (setcar command binary)))))
      (when (or (memq 'input command) (memq 'inplace command))
        (let ((input-fname (make-temp-file
                            "apheleia" nil
                            (when-let ((file-name
                                        (or buffer-file-name
                                            (apheleia--safe-buffer-name))))
                              (file-name-extension file-name 'period)))))
          (with-current-buffer stdin
            (apheleia--write-region-silently nil nil input-fname))
          (setq command (mapcar (lambda (arg)
                                  (if (memq arg '(input inplace))
                                      input-fname
                                    arg))
                                command))
          (when (memq 'inplace command)
            (setq output-fname input-fname))))
      (when (memq 'output command)
        (setq output-fname (make-temp-file "apheleia"))
        (setq command (mapcar (lambda (arg)
                                (if (eq arg 'output)
                                    output-fname
                                  arg))
                              command)))
      (when (memq 'file command)
        ;; Fail when using file but not as the first formatter in this
        ;; sequence.
        (when stdin-buffer
          (error "Cannot run formatter using `file' in a \
sequence unless it's first in the sequence"))
        (let ((file-name (or buffer-file-name
                             (concat default-directory
                                     (apheleia--safe-buffer-name)))))
          (setq command (mapcar (lambda (arg)
                                  (when (eq arg 'file)
                                    (setq stdin nil))
                                  (if (memq arg '(file filepath))
                                      (prog1 file-name
                                        (when (buffer-modified-p)
                                          (cl-return)))
                                    arg))
                                command))))
      ;; Evaluate each element of arg that isn't a string and replace
      ;; it with the evaluated value. The result of an evaluation should
      ;; be a string or a list of strings. If the former its replaced as
      ;; is. If the latter the contents of the list is substituted in
      ;; place.
      (setq command
            (cl-loop
             for arg in command
             with val = nil
             do (setq val (if (stringp arg)
                              arg
                            (eval arg)))
             if val
               if (and (consp val)
                       (cl-every #'stringp val))
                 append val
               else if (stringp val)
                 collect val
               else do (error "Result of command evaluation must be a string \
or list of strings: %S" arg)))
      `(,input-fname ,output-fname ,stdin ,@command))))

(defun apheleia--run-formatter-command
    (command buffer callback stdin formatter)
  "Run a formatter using a shell command.
COMMAND should be a list of string or symbols for the formatter that
will format the current buffer. See `apheleia--run-formatters' for a
description of COMMAND, BUFFER, CALLBACK and STDIN. FORMATTER is
the symbol of the current formatter being run, for diagnostic
purposes."
  ;; NOTE: We switch to the original buffer both to format the command
  ;; correctly and also to ensure any buffer local variables correctly
  ;; resolve for the whole formatting process (for example
  ;; `apheleia--current-process').
  (with-current-buffer buffer
    (when-let ((ret (apheleia--format-command command stdin)))
      (cl-destructuring-bind (input-fname output-fname stdin &rest command) ret
        (apheleia--make-process
         :command command
         :stdin (unless input-fname
                  stdin)
         :callback
         (lambda (stdout)
           (when output-fname
             ;; Load output-fname contents into the stdout buffer.
             (erase-buffer)
             (insert-file-contents-literally output-fname))

           (funcall callback stdout))
         :ensure
         (lambda ()
           (ignore-errors
             (when input-fname
               (delete-file input-fname))
             (when output-fname
               (delete-file output-fname))))
         :formatter formatter)))))

(defun apheleia--run-formatter-function (func buffer callback stdin _formatter)
  "Run a formatter using a Lisp function FUNC.
See `apheleia--run-formatters' for a description of BUFFER,
CALLBACK and STDIN. FORMATTER is the symbol of the current
formatter being run, for diagnostic purposes."
  ;; Will be an ugly name if you use a lambda for FUNC, instead of a symbol.
  (let* ((formatter-name (if (symbolp func) (symbol-name func) "lambda"))
         (scratch (generate-new-buffer
                   (format " *apheleia-%s-scratch*" formatter-name))))
    (with-current-buffer scratch
      ;; We expect FUNC to modify scratch in place so we can't simply pass
      ;; STDIN to it. When STDIN isn't nil, it's the output of a previous
      ;; formatter and we want to keep it alive so we can debug any issues
      ;; with it.
      (insert-buffer-substring (or stdin buffer))
      (funcall func
               ;; Original buffer being formatted.
               buffer
               ;; Buffer the formatter should modify.
               scratch
               ;; Callback after succesfully formatting.
               (lambda ()
                 (unwind-protect
                     (funcall callback scratch)
                   (kill-buffer scratch)))
               ;; Callback when formatting scratch has failed.
               (apply-partially #'kill-buffer scratch)))))

(defcustom apheleia-formatters
  '((black . ("black" "-"))
    (brittany . ("brittany"))
    (clang-format . ("clang-format"))
    (fish-indent . ("fish_indent"))
    (gofmt . ("gofmt"))
    (google-java-format . ("google-java-format" "-"))
    (isort . ("isort" "--stdout" "-"))
    (latexindent . ("latexindent"))
    (mix-format . ("mix" "format" "-"))
    (ocamlformat . ("ocamlformat" "-" "--name" filepath))
    (prettier . (npx "prettier" "--stdin-filepath" filepath))
    (rustfmt . ("rustfmt" "--quiet" "--emit" "stdout"))
    (terraform . ("terraform" "fmt" "-")))
  "Alist of code formatting commands.
The keys may be any symbols you want, and the values are
shell commands, lists of strings and symbols, or a function
symbol.

If the value is a function, the function will be called with four
arguments to format the current buffer: the original buffer that
was being formatted (use this to access any relevent local
variables or options that the formatter needs); a clone of the
original buffer (that may have been modified by another formatter
prior to being passed to the function); a callback that should be
called when formatting is finished; and another callback that
should be called when an error was raised during formatting.

Otherwise in Lisp code, the format of commands is similar to what
you pass to `make-process', except as follows.

Normally, the contents of the current buffer are passed to the
command on stdin, and the output is read from stdout. However, if
you use the symbol `file' as one of the elements of commands,
then the filename of the current buffer is substituted for
it. (Use `filepath' instead of `file' if you need the filename of
the current buffer, but you still want its contents to be passed
on stdin.)

If you instead use the symbol `input' as one of the elements of
commands, then the contents of the current buffer are written to
a temporary file and its name is substituted for `input'. Also,
if you use the symbol `output' as one of the elements of
commands, then it is substituted with the name of a temporary
file. In that case, it is expected that the command writes to
that file, and the file is then read into an Emacs buffer.

If you use the symbol `inplace' as one of the elements of the
list, then the contents of the current buffer are written to a
temporary file and its name is substituted for `inplace'.
However, unlike `input', it is expected that the formatter write
the formatted file back to the same file in place. In other
words, `inplace' is like `input' and `output' together.

If you use the symbol `npx' as one of the elements of commands,
then the first string element of the command list is resolved
inside node_modules/.bin if such a directory exists anywhere
above the current `default-directory'."
  :type '(alist
          :key-type symbol
          :value-type
          (choice
           (repeat
            (choice
             (string :tag "Argument")
             (const :tag "Look for command in node_modules/.bin" npx)
             (const :tag "Name of file being formatted" filepath)
             (const :tag "Name of real file used for input" file)
             (const :tag "Name of temporary file used for input" input)
             (const :tag "Name of temporary file used for output" output)))
           (function :tag "Formatter function"))))

(defun apheleia--run-formatters
    (formatters buffer callback &optional stdin)
  "Run one or more code formatters on the current buffer.
FORMATTERS is a list of symbols that appear as keys in
`apheleia-formatters'. BUFFER is the `current-buffer' when this
function was first called. Once all the formatters in COMMANDS
finish succesfully then invoke CALLBACK with one argument, a
buffer containing the output of all the formatters.

STDIN is a buffer containing the standard input for the first
formatter in COMMANDS. This should not be supplied by the caller
and instead is supplied by this command when invoked recursively.
The stdout of the previous formatter becomes the stdin of the
next formatter."
  (let ((command (alist-get (car formatters) apheleia-formatters)))
    (funcall
     (cond
      ((consp command)
       #'apheleia--run-formatter-command)
      ((or (functionp command)
           (symbolp command))
       #'apheleia--run-formatter-function)
      (t
       (error "Formatter must be a shell command or a Lisp \
function: %s" command)))
     command
     buffer
     (lambda (stdout)
       (if (cdr formatters)
           ;; Forward current stdout to remaining formatters, passing along
           ;; the current callback and using the current formatters output
           ;; as stdin.
           (apheleia--run-formatters (cdr formatters) buffer callback stdout)
         (funcall callback stdout)))
     stdin
     (car formatters))))

(defcustom apheleia-mode-alist
  '((cc-mode . clang-format)
    (c-mode . clang-format)
    (c++-mode . clang-format)
    (css-mode . prettier)
    (elixir-mode . mix-format)
    (fish-mode . fish-indent)
    (go-mode . gofmt)
    (haskell-mode . brittany)
    (html-mode . prettier)
    (java-mode . google-java-format)
    (js3-mode . prettier)
    (js-mode . prettier)
    (json-mode . prettier)
    (latex-mode . latexindent)
    (LaTeX-mode . latexindent)
    (php-mode . nil)
    (python-mode . black)
    (ruby-mode . prettier)
    (rustic-mode . rustfmt)
    (rust-mode . rustfmt)
    (sass-mode . prettier)
    (terraform-mode . terraform)
    (TeX-latex-mode . latexindent)
    (TeX-mode . latexindent)
    (tuareg-mode . ocamlformat)
    (typescript-mode . prettier)
    (web-mode . prettier)
    (yaml-mode . prettier))
  "Alist mapping major mode names to formatters to use in those modes.
This determines what formatter to use in buffers without a
setting for `apheleia-formatter'. The keys are major mode
symbols (matched against `major-mode' with `derived-mode-p') or
strings (matched against value of variable `buffer-file-name'
with `string-match-p'), and the values are symbols with entries
in `apheleia-formatters' (or equivalently, they are allowed
values for `apheleia-formatter'). Values can be a list of such
symnols causing each formatter in the list to be called one after
the other (with the output of the previous formatter).
Earlier entries in this variable take precedence over later ones.

Be careful when writing regexps to include \"\\'\" and to escape
\"\\.\" in order to properly match a file extension. For example,
to match \".jsx\" files you might use \"\\.jsx\\'\"."
  :type '(alist
          :key-type
          (choice (symbol :tag "Major mode")
                  (string :tag "Buffer name regexp"))
          :value-type
          (choice (symbol :tag "Formatter")
                  (repeat
                   (symbol :tag "Formatter")))))

(defvar-local apheleia-formatter nil
  "Name of formatter to use in current buffer, a symbol or nil.
If non-nil, then `apheleia-formatters' should have a matching
entry. This overrides `apheleia-mode-alist'.")

(defun apheleia--ensure-list (arg)
  "Ensure ARG is a list of length at least 1.
When ARG is not a list its turned into a list."
  (if (listp arg)
      arg
    (list arg)))

(defun apheleia--get-formatters (&optional interactive)
  "Return the list of formatters to use for the current buffer.
This is a list of symbols that may appear as cars in
`apheleia-formatters', or nil if no formatter is configured for
the current buffer.

Consult the values of `apheleia-mode-alist' and
`apheleia-formatter' to determine which formatter is configured.

If INTERACTIVE is non-nil, then prompt the user for which
formatter to run if none is configured, instead of returning nil.
If INTERACTIVE is the special symbol `prompt', then prompt
even if a formatter is configured."
  (or (and (not (eq interactive 'prompt))
           (apheleia--ensure-list
            (or apheleia-formatter
                (cl-dolist (entry apheleia-mode-alist)
                  (when (or (and (symbolp (car entry))
                                 (derived-mode-p (car entry)))
                            (and (stringp (car entry))
                                 buffer-file-name
                                 (string-match-p
                                  (car entry) buffer-file-name)))
                    (cl-return (cdr entry)))))))
      (and interactive
           (list
            (intern
             (completing-read
              "Formatter: "
              (or (map-keys apheleia-formatters)
                  (user-error
                   "No formatters in `apheleia-formatters'"))
              nil 'require-match))))))

(defun apheleia--buffer-hash ()
  "Compute hash of current buffer."
  (if (fboundp 'buffer-hash)
      (buffer-hash)
    (md5 (current-buffer))))

(defvar apheleia--buffer-hash nil
  "Return value of `buffer-hash' when formatter started running.")

(defun apheleia--disallowed-p ()
  "Return an error message if Apheleia cannot be run, else nil."
  (when (and buffer-file-name (file-remote-p buffer-file-name))
    "Apheleia does not support remote files"))

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
    (unless (apheleia--disallowed-p)
      (setq-local apheleia--buffer-hash (apheleia--buffer-hash))
      (let ((cur-buffer (current-buffer)))
        (apheleia--run-formatters
         formatters
         cur-buffer
         (lambda (formatted-buffer)
           (with-current-buffer cur-buffer
             ;; Short-circuit.
             (when (equal apheleia--buffer-hash (apheleia--buffer-hash))
               (apheleia--create-rcs-patch
                (current-buffer) formatted-buffer
                (lambda (patch-buffer)
                  (with-current-buffer cur-buffer
                    (when (equal apheleia--buffer-hash (apheleia--buffer-hash))
                      (apheleia--apply-rcs-patch
                       (current-buffer) patch-buffer)
                      (when callback
                        (funcall callback))))))))))))))

(defcustom apheleia-post-format-hook nil
  "Normal hook run after Apheleia formats a buffer successfully."
  :type 'hook)

;; Handle recursive references.
(defvar apheleia-mode)

;; Prevent infinite loop.
(defvar apheleia--format-after-save-in-progress nil
  "Prevent `apheleia--format-after-save' from being called recursively.
This will be locally bound to t while `apheleia--format-after-save' is
operating, to prevent an infinite loop.")

;; Autoload because the user may enable `apheleia-mode' without
;; loading Apheleia; thus this function may be invoked as an autoload.
;;;###autoload
(defun apheleia--format-after-save ()
  "Run code formatter for current buffer if any configured, then save."
  (unless apheleia--format-after-save-in-progress
    (when apheleia-mode
      (when-let ((formatters (apheleia--get-formatters)))
        (apheleia-format-buffer
         formatters
         (lambda ()
           (with-demoted-errors "Apheleia: %s"
             (when buffer-file-name
               (let ((apheleia--format-after-save-in-progress t))
                 (apheleia--write-file-silently buffer-file-name)))
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
    :lighter " Apheleia"
    (if apheleia-mode
        (add-hook 'after-save-hook #'apheleia--format-after-save nil 'local)
      (remove-hook 'after-save-hook #'apheleia--format-after-save 'local)))

  (define-globalized-minor-mode apheleia-global-mode
    apheleia-mode apheleia-mode)

  (put 'apheleia-mode 'safe-local-variable #'booleanp))

(provide 'apheleia)

;;; apheleia.el ends here
