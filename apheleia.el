;;; apheleia.el --- Reformat buffer stably -*- lexical-binding: t -*-

;; Copyright (C) 2019-2022 Radian LLC and contributors

;; Author: Radian LLC <contact+apheleia@radian.codes>
;; Created: 7 Jul 2019
;; Homepage: https://github.com/raxod502/apheleia
;; Keywords: tools
;; Package-Requires: ((emacs "26"))
;; SPDX-License-Identifier: MIT
;; Version: 3.2

;;; Commentary:

;; Apheleia is an Emacs Lisp package which allows you to reformat a
;; buffer without moving point. This solves the usual problem of
;; running a tool like Prettier or Black on `before-save-hook', namely
;; that it resets point to the beginning of the buffer. Apheleia
;; maintains the position of point relative to its surrounding text
;; even if the buffer is modified by the reformatting.

;; Please see https://github.com/raxod502/apheleia for more information.

;;; Code:

(defgroup apheleia nil
  "Reformat buffer without moving point."
  :group 'external
  :link '(url-link :tag "GitHub" "https://github.com/raxod502/apheleia")
  :link '(emacs-commentary-link :tag "Commentary" "apheleia"))

(provide 'apheleia)

;;; apheleia.el ends here
