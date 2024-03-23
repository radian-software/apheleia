;; -*- lexical-binding: t -*-

;; `apheleia-unit-tests' - unit tests using ERT.

(require 'apheleia)

;; Using buttercup because ert makes it really hard to write tabular
;; tests that report enough context to debug when they fail.
(require 'buttercup)

(defun apheleia-unit-find-vars (form)
  (cond
   ((symbolp form)
    (list form))
   ((listp form)
    (mapcan #'apheleia-unit-find-vars (cdr form)))))

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
       ("hello| world"
        "helo| word")
       ("hel|lo"
        "he|o")
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
        "<|div class=\"left-[40rem] fixed inset-y-0 right-0 z-0 hidden lg:block xl:left-[50rem]\">\n <svg")))))
