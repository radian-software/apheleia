;; -*- lexical-binding: t -*-

;; `apheleia-unit' - unit tests using ERT.

(require 'apheleia)

(require 'ert)

(defun apheleia-unit-find-vars (form)
  (cond
   ((symbolp form)
    (list form))
   ((listp form)
    (mapcan #'apheleia-unit-find-vars (cdr form)))))

(ert-deftest apheleia-align-point ()
  (dolist (testcase
           '(("hello| world"
              "helo| word")
             ("hello | world"
              "hello|world")
             ;; https://github.com/radian-software/apheleia/pull/290
             ("      | <div class=\"left-[40rem] fixed inset-y-0 right-0 z-0 hidden lg:block xl:left-[50rem]\">\n  <svg\n"
              "<|div class=\"left-[40rem] fixed inset-y-0 right-0 z-0 hidden lg:block xl:left-[50rem]\">\n <svg")))
    (save-match-data
      (cl-destructuring-bind (before-spec after-spec) testcase
        (let ((before-pos (string-match "|" before-spec))
              (after-pos (string-match "|" after-spec))
              (before-str (replace-regexp-in-string "|" "" before-spec))
              (after-str (replace-regexp-in-string "|" "" after-spec)))
          (should
           (equal (apheleia--align-point before-str after-str before-pos) after-pos)))))))
