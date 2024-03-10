;; -*- lexical-binding: t -*-

;; `apheleia-unit' - unit tests using ERT.

(require 'apheleia)

(require 'ert)

(ert-deftest apheleia-align-point ()
  ;; https://github.com/radian-software/apheleia/pull/290
  (should
   (equal
    (apheleia--align-point
     "       <div class=\"left-[40rem] fixed inset-y-0 right-0 z-0 hidden lg:block xl:left-[50rem]\">\n  <svg\n"
     "<div class=\"left-[40rem] fixed inset-y-0 right-0 z-0 hidden lg:block xl:left-[50rem]\">\n <svg"
     6)
    1)))
