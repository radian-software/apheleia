;; -*- lexical-binding: t -*-

;; `apheleia-it' - short for `apheleia-integration-tests'. The
;; functions in here are not part of the public interface of Apheleia
;; and breaking changes may occur at any time.

(require 'apheleia)

(require 'cl-lib)

(defvar apheleia-it-mode-keymap
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "q") #'quit-window)))
  "Keymap for use in `apheleia-it-mode'.")

(define-minor-mode apheleia-it-mode
  "Minor mode to add some keybindings in test result buffers."
  :keymap apheleia-it-mode-keymap)

(defvar apheleia-it-tests nil
  "List of integration tests, an alist.")
(setq apheleia-it-tests nil)

(cl-defmacro apheleia-it-deftest (name desc &rest kws &key scripts formatters steps)
  "Declare a integration test."
  (declare (indent defun) (doc-string 2))
  (ignore scripts formatters steps)
  `(progn
     (when (alist-get ',name apheleia-it-tests)
       (message "Overwriting existing test: %S" ',name))
     (setf (alist-get ',name apheleia-it-tests) (cl-list* :desc ,desc ,@kws))))

(defvar apheleia-it-workdir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory that this variable is defined in.")

(defun apheleia-it--run-test-steps (steps callback bindings)
  "Run STEPS from defined integration test.
This is a list that can appear in `:steps'. For supported steps,
see the implementation below, or example tests. CALLBACK will be
invoked, with nil or an error, after the steps are run. This
could be synchronous or asynchronous. BINDINGS is a `let'-style
list of lexical bindings that will be available for `eval'
steps."
  (pcase steps
    (`nil (funcall callback))
    (`((with-callback ,callback . ,body) . ,rest)
     (apheleia-it--run-test-steps
      body
      #'ignore
      (cons
       (list callback (lambda ()
                        (apheleia-it--run-test-steps
                         rest callback bindings)))
       bindings)))
    (`((eval ,form))
     (eval
      `(let (,@bindings)
         ,form))
     (funcall callback))
    (`((insert ,str) . ,rest)
     (erase-buffer)
     (let ((p (string-match-p "|" str)))
       (insert (replace-regexp-in-string "|" "" str nil 'literal))
       (goto-char p))
     (apheleia-it--run-test-steps rest callback bindings))
    (`((expect ,str) . ,rest)
     (cl-assert (eq (point) (string-match-p "|" str)))
     (cl-assert
      (string=
       (buffer-string)
       (replace-regexp-in-string "|" "" str nil 'literal))))))

(defun apheleia-it-run-test (name)
  "Run a single integration test. Return non-nil if passed, nil if failed."
  (interactive
   (list
    (intern
     (completing-read
      "Run test: "
      (mapcar #'symbol-name (map-keys apheleia-it-tests))))))
  (let* ((test (alist-get name apheleia-it-tests))
         (bufname (format " *apheleia-it test %S*" name))
         (result nil))
    (unless (plist-get test :steps)
      (user-error "Incomplete test: %S" name))
    (when (get-buffer bufname)
      (kill-buffer bufname))
    (cl-block nil
      (save-window-excursion
        (pop-to-buffer bufname)
        (setq-local default-directory apheleia-it-workdir)
        (fundamental-mode)
        (apheleia-it-mode +1)
        (ignore-errors
          (delete-directory ".tmp" 'recursive))
        (make-directory ".tmp")
        (dolist (script (plist-get test :scripts))
          (with-temp-buffer
            (insert (cdr script))
            (write-file (format ".tmp/%s" (car script)))))
        (setq-local apheleia-formatters (plist-get test :formatters))
        (apheleia-it--run-test-steps (plist-get test :steps) FIXME)
        (search-forward "|")
        (delete-region (match-beginning 0) (match-end 0))
        (condition-case e
            (execute-kbd-macro (kbd (plist-get test :keys)))
          (error
           (save-excursion
             (goto-char (point-max))
             (insert " [" (error-message-string e) "]"))))
        (insert "|")
        (setq result (buffer-string))))
    (if (equal result (plist-get test :after))
        (progn
          (message "Test %S passed" name)
          t)
      (message "Test %S failed" name)
      (with-current-buffer bufname
        (erase-buffer)
        (insert
         "TEST:\n\n"
         (symbol-name name)
         "\n"
         (plist-get test :desc)
         "\n\nBEFORE:\n\n"
         (plist-get test :before)
         "\n\nKEYS:\n\n"
         (plist-get test :keys)
         "\n\nEXPECTED:\n\n"
         (plist-get test :after)
         "\n\nGOT:\n\n"
         result
         "\n")
        (apheleia-it-mode +1))
      (if noninteractive
          (progn
            (message "%s" (with-current-buffer bufname
                            (string-trim (buffer-string))))
            (kill-emacs 1))
        (pop-to-buffer bufname))
      nil)))

(defun apheleia-it-run-all-tests ()
  "Run all the integration tests until a failure is encountered."
  (interactive)
  (cl-block nil
    (dolist (name (nreverse (map-keys apheleia-it-tests)))
      (unless (apheleia-it-run-test name)
        (cl-return)))
    (message "All tests passed")))

(cl-defun apheleia-it-script (&key allowed-inputs)
  "Return text of a bash script to act as a mock formatter.
Keyword arguments control the behavior. ALLOWED-INPUTS is an
alist of inputs that are allowed to be passed to the formatter,
along with the outputs that is will return. Any other input will
generate an error."
  (concat
   "#!/usr/bin/env bash
input=\"$(cat)\"
"
   (mapcan
    (lambda (link)
      (cl-destructuring-bind (input . output) link
        (format
         "expected_input=%s
expected_output=%s
if [[ \"${input}\" == \"${expected_input}\" ]]; then
    printf '%%s' \"${expected_output}\"
    exit 0
fi
"
         (shell-quote-argument input)
         (shell-quote-argument output))))
    allowed-inputs)
   "echo >&2 'formatter got unexpected input'
exit 1
"))

(apheleia-it-deftest basic-functionality
  "Running `apheleia-format-buffer' does formatting"
  :scripts `(("apheleia-it" .
              ,(apheleia-it-script
                :allowed-inputs
                '(("The quick brown fox jumped over the lazy dog\n" .
                   "The slow brown fox jumped over the studious dog\n")))))
  :formatters '((apheleia-it . ("apheleia-it")))
  :steps '((insert "The quick brown fox jum|ped over the lazy dog\n")
           (with-callback
            callback
            (eval (apheleia-format-buffer 'apheleia-it callback)))
           (expect "The slow brown fox jum|ped over the studious dog\n")))
