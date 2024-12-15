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

(cl-defmacro apheleia-it-deftest
    (name desc &rest kws &key scripts formatters steps)
  "Declare a integration test."
  (declare (indent defun) (doc-string 2))
  (ignore scripts formatters steps)
  `(progn
     (when (alist-get ',name apheleia-it-tests)
       (message "Overwriting existing test: %S" ',name))
     (setf (alist-get ',name apheleia-it-tests) (list :desc ,desc ,@kws))))

(defvar apheleia-it-workdir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory that this variable is defined in.")

(defvar apheleia-it-timers nil
  "List of timers that should be canceled or finished before exit.")

(defun apheleia-it-run-with-timer (secs function &rest args)
  "Like `run-with-timer' but delays Emacs exit until done or canceled."
  (let ((timer (apply #'run-with-timer secs nil function args)))
    (prog1 timer
      (push timer apheleia-it-timers))))

(defun apheleia-it-timers-active-p ()
  "Non-nil if there are any active Apheleia timers for tests.
This may mutate `apheleia-it-timers' to cleanup expired timers."
  (cl-block nil
    (while apheleia-it-timers
      (if (memq (car apheleia-it-timers) timer-list)
          (cl-return t)
        (setq apheleia-it-timers (cdr apheleia-it-timers))))))

(defun apheleia-it--run-test-steps (steps bindings callback)
  "Run STEPS from defined integration test.
This is a list that can appear in `:steps'. For supported steps,
see the implementation below, or example tests. BINDINGS is a
`let'-style list of lexical bindings that will be available for
`eval' steps. CALLBACK will be invoked, with nil or an error,
after the steps are run. This could be synchronous or
asynchronous."
  (apheleia--log
   'test "Running test step %s"
   (replace-regexp-in-string
    "\n" "\\n" (format "%S" (car steps)) nil 'literal))
  (condition-case-unless-debug err
      (pcase steps
        (`nil (funcall callback nil))
        (`((with-callback ,callback-sym . ,body) . ,rest)
         (let* ((callback-called nil)
                (timeout-timer nil)
                (wrapped-callback
                 (lambda (err)
                   (when (timerp timeout-timer)
                     (cancel-timer timeout-timer))
                   (unless callback-called
                     (setq callback-called t)
                     (if err
                         (funcall callback err)
                       (apheleia-it--run-test-steps
                        rest bindings callback))))))
           (setq timeout-timer
                 (apheleia-it-run-with-timer
                  3 wrapped-callback
                  (cons 'error (format
                                "Callback not invoked within timeout for %S"
                                body))))
           (apheleia-it--run-test-steps
            body
            (cons
             (cons callback-sym
                   wrapped-callback)
             bindings)
            #'ignore)))
        (`((eval ,form))
         (eval form bindings)
         (funcall callback nil))
        (`((insert ,str) . ,rest)
         (erase-buffer)
         (let ((p (string-match-p "|" str)))
           (insert (replace-regexp-in-string "|" "" str nil 'literal))
           (goto-char p))
         (apheleia-it--run-test-steps rest bindings callback))
        (`((expect ,str) . ,rest)
         (cl-assert (eq (point) (string-match-p "|" str)))
         (cl-assert
          (string=
           (buffer-string)
           (replace-regexp-in-string "|" "" str nil 'literal)))
         (funcall callback nil))
        (_ (error "Malformed test step `%S'" (car steps))))
    (error (funcall callback err))))

(defun apheleia-it-run-test (name callback)
  "Run a single integration test. Invoke CALLBACK with nil or an error."
  (interactive
   (list
    (intern
     (completing-read
      "Run test: "
      (mapcar #'symbol-name (map-keys apheleia-it-tests))))
    (lambda (err)
      (if err
          (signal (car err) (cdr err))
        (message "Test passed" (length apheleia-it-tests))))))
  (message "Running test %S" name)
  (condition-case-unless-debug err
      (let* ((test (alist-get name apheleia-it-tests))
             (bufname (format " *apheleia-it test %S*" name))
             (result nil))
        (unless (plist-get test :steps)
          (user-error "Incomplete test: %S" name))
        (when (get-buffer bufname)
          (kill-buffer bufname))
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
            (let ((fname (expand-file-name (format ".tmp/%s" (car script)))))
              (write-file fname)
              (chmod fname #o755))))
        (setq-local exec-path (cons (expand-file-name ".tmp") exec-path))
        (setq-local apheleia-formatters (plist-get test :formatters))
        (apheleia-it--run-test-steps (plist-get test :steps) nil callback))
    (error (funcall callback err))))

(defun apheleia-it-run-tests (names callback)
  "Run multiple integration tests. Stop on error.
Invoke CALLBACK with nil or an error."
  (if names
      (apheleia-it-run-test
       (car names)
       (lambda (err)
         (if err
             (funcall callback err)
           (apheleia-it-run-tests (cdr names) callback))))
    (funcall callback nil)))

(defun apheleia-it-run-all-tests ()
  "Run all the integration tests until a failure is encountered."
  (interactive)
  (apheleia-it-run-tests
   (nreverse (map-keys apheleia-it-tests))
   (lambda (err)
     (if err
         (signal (car err) (cdr err))
       (message "All %d tests passed" (length apheleia-it-tests)))))
  (when noninteractive
    (while (apheleia-it-timers-active-p)
      (sit-for 0.5))))

(cl-defun apheleia-it-script (&key allowed-inputs)
  "Return text of a bash script to act as a mock formatter.
Keyword arguments control the behavior. ALLOWED-INPUTS is an
alist of inputs that are allowed to be passed to the formatter,
along with the outputs that is will return. Any other input will
generate an error."
  (concat
   "#!/usr/bin/env bash
input=\"$(cat; echo x)\"
input=\"${input%x}\"
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
echo >&2 'received input follows:'
echo \"${input}\" | sed 's/^/| /' >&2
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
            (eval (apheleia-format-buffer
                   'apheleia-it nil
                   :callback
                   (lambda (&rest props)
                     (funcall callback (plist-get props :error))))))
           (expect "The slow brown fox jum|ped over the studious dog\n")))
