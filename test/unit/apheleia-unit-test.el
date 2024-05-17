;; -*- lexical-binding: t -*-

;; `apheleia-unit-tests' - unit tests using ERT.

(require 'apheleia)

;; Using buttercup because ert makes it really hard to write tabular
;; tests that report enough context to debug when they fail.
(require 'buttercup)

(require 'map)

(defun apheleia-unit-find-vars (form)
  (cond
   ((symbolp form)
    (list form))
   ((listp form)
    (mapcan #'apheleia-unit-find-vars (cdr form)))))

(describe "apheleia--edit-distance-table"
  (cl-flet ((table-error
             (before-str after-str expected-table)
             (let* ((hash (apheleia--edit-distance-table before-str after-str))
                    (table
                     (mapcar
                      (lambda (i2)
                        (mapcar
                         (lambda (i1)
                           (gethash (cons i1 i2) hash))
                         (number-sequence 0 (length before-str))))
                      (number-sequence 0 (length after-str)))))
               (unless (equal table expected-table)
                 table))))
    (cl-macrolet ((testcases
                   (description &rest specs)
                   `(it ,description
                      ,@(mapcar
                         (lambda (spec)
                           `(expect
                             (table-error ,@spec)
                             :to-be nil))
                         specs))))
      (testcases
       "computes the example from apheleia-dp file header"
       ("hello" "heo"
        '((0 1 2 3 4 5)
          (1 0 1 2 3 4)
          (2 1 0 1 2 3)
          (3 2 1 1 2 2)))))))

(describe "apheleia--align-point"
  (cl-flet ((alignment-error
             (before-spec after-spec)
             (let* ((before-pos (string-match "|" before-spec))
                    (after-pos (string-match "|" after-spec))
                    (before-str (replace-regexp-in-string "|" "" before-spec))
                    (after-str (replace-regexp-in-string "|" "" after-spec))
                    (real-after-pos (apheleia--align-point before-str after-str before-pos)))
               (unless (= after-pos real-after-pos)
                 (concat (substring after-str 0 real-after-pos) "|"
                         (substring after-str real-after-pos))))))
    (cl-macrolet ((testcases
                   (description &rest specs)
                   `(it ,description
                      ,@(mapcar
                         (lambda (spec)
                           (cl-destructuring-bind (before-spec after-spec) spec
                             `(expect
                               (alignment-error ,before-spec ,after-spec)
                               :to-be nil)))
                         specs))))
      (testcases
       "does normal alignments"
       ("hel|lo"
        "he|o")
       ("hello| world"
        "helo| word")
       ("hello | world"
        "hello|world"))
      (testcases
       "solves issue #2"
       ("  if (node.type === \"CallExpression\" && (node.callee.type === \"Import\" @@ (node.callee.type === \"Identifier\" && node.callee.name === \"require\"))) {
    //|
  }
"
        "  if (
    node.type === \"CallExpression\" &&
    (node.callee.type === \"Import\" @@
      (node.callee.type === \"Identifier\" && node.callee.name === \"require\"))
  ) {
    //|
  }
"))
      (testcases
       "solves issue #290"
       ("      | <div class=\"left-[40rem] fixed inset-y-0 right-0 z-0 hidden lg:block xl:left-[50rem]\">\n  <svg\n"
        "|<div class=\"left-[40rem] fixed inset-y-0 right-0 z-0 hidden lg:block xl:left-[50rem]\">\n <svg")))))

(describe "apheleia--get-formatters"
  (cl-macrolet ((testcases
                 (description mode-alist pred-list &rest specs)
                 `(cl-flet ((fmt-error
                             (mode fname expected)
                             (with-temp-buffer
                               (setq major-mode mode)
                               (setq-local buffer-file-name fname)
                               (let ((real (apheleia--get-formatters)))
                                 (unless (equal real expected)
                                   real)))))
                    (it ,description
                      ,@(mapcar
                         (lambda (spec)
                           `(let ((apheleia-mode-alist ,mode-alist)
                                  (apheleia-mode-predicates ,pred-list))
                              (expect
                               (fmt-error ,@spec)
                               :to-be nil)))
                         specs)))))
    (testcases
     "always returns nil when user options are nil"
     nil nil
     ('text-mode "foo.txt" nil)
     ('fundamental-mode nil nil)
     ('cc-mode "foo.c" nil)
     ('mhtml-mode "foo.html" nil))
    (testcases
     "selects based on mode, filename, and predicates"
     '(("\\.foobar\\'" . fmt-foobar)
       (sgml-mode . fmt-sgml)
       (html-mode . (fmt-html fmt-html-again))
       (text-mode . fmt-text)
       (blah-mode . fmt-blah)
       ("\\.foobaz\\'" . fmt-foobaz))
     '((lambda ()
         (when (and buffer-file-name
                    (string-match-p "\\.blah" buffer-file-name))
           'blah-mode)))
     ('fundamental-mode nil nil)
     ('fundamental-mode "ok.foobar" '(fmt-foobar))
     ('text-mode nil '(fmt-text))
     ('sgml-mode nil '(fmt-sgml))
     ('html-mode nil '(fmt-html fmt-html-again))
     ('html-mode "ok.foobar" '(fmt-foobar))
     ('html-mode "ok.foobaz" '(fmt-html fmt-html-again))
     ('html-mode "ok.blah" '(fmt-blah))
     ('html-mode "ok.blah.foobar" '(fmt-foobar))
     ('html-mode "ok.blah.foobaz" '(fmt-blah)))))
