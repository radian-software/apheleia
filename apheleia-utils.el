;;; apheleia-utils.el --- Formatter helpers. -*- lexical-binding: t -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Helper functions for defining apheleia formatters.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom apheleia-formatters-respect-indent-level t
  "Whether formatters should respect Emacs' indent configuration."
  :type 'boolean
  :group 'apheleia
  :safe #'booleanp)

(defun apheleia-formatters-indent (tab-flag indent-flag &optional indent-var)
  "Set flag for indentation.
Helper function for `apheleia-formatters' which allows you to supply
alternating flags based on the current buffers indent configuration. If the
buffer is indented with tabs then returns TAB-FLAG. Otherwise if INDENT-VAR
is set in the buffer return INDENT-FLAG and the value of INDENT-VAR. Use this
to easily configure the indentation level of a formatter. If INDENT-VAR is
unset then intelligently try to determine the indentation variable based on
the current mode.

If `apheleia-formatters-respect-indent-level' is nil then this
always returns nil to defer to the formatter."
  (cond
   ((not apheleia-formatters-respect-indent-level) nil)
   (indent-tabs-mode tab-flag)
   (t
    (unless indent-var
      (setq indent-var
            (cl-case major-mode
              (cperl-mode 'cperl-indent-level)
              (css-mode 'css-indent-offset)
              (css-ts-mode 'css-indent-offset)
              (graphql-mode 'graphql-indent-level)
              (html-mode 'sgml-basic-offset)
              (js-jsx-mode 'js-indent-level)
              (js-ts-mode 'js-indent-level)
              (js-mode 'js-indent-level)
              (js2-jsx-mode 'js2-basic-offset)
              (js2-mode 'js2-basic-offset)
              (js3-mode 'js3-indent-level)
              (json-mode 'js-indent-level)
              (json-ts-mode 'json-ts-mode-indent-offset)
              (nxml-mode 'nxml-child-indent)
              (robot-mode 'robot-mode-basic-offset)
              (perl-mode 'perl-indent-level)
              (python-mode 'python-indent-offset)
              (ruby-mode 'ruby-indent-level)
              (ruby-ts-mode 'ruby-indent-level)
              (scss-mode 'css-indent-offset)
              (svelte-mode 'svelte-basic-offset)
              (web-mode 'web-mode-indent-style)
              (tsx-ts-mode 'typescript-ts-mode-indent-offset)
              (typescript-mode 'typescript-indent-level)
              (typescript-ts-mode 'typescript-ts-mode-indent-offset)
              (yaml-mode 'yaml-indent-offset))))

    (when-let ((indent (and indent-var
                            (boundp indent-var)
                            (symbol-value indent-var))))
      (list indent-flag (number-to-string indent))))))

(define-obsolete-function-alias 'apheleia-formatters-js-indent
  'apheleia-formatters-indent "4.1")

(defcustom apheleia-formatters-respect-fill-column nil
  "Whether formatters should set `fill-column' related flags."
  :type 'boolean
  :group 'apheleia
  :safe #'booleanp)

(defun apheleia-formatters-fill-column (fill-flag)
  "Set flag for wrap column.
Helper function to set a flag based on `fill-column'. When `fill-column' is set
and `apheleia-formatters-respect-fill-column' return a list of FILL-FLAG and
`fill-column'."
  (when (and apheleia-formatters-respect-fill-column
             (bound-and-true-p fill-column))
    (list fill-flag (number-to-string fill-column))))

(defun apheleia-formatters-locate-file (file-flag file-name)
  "Set a flag based on a dominating-file.
Look for a file up recursively from the current directory until FILE-NAME is
found. If found return a list of FILE-FLAG and the absolute path to the located
FILE-NAME."
  (when-let ((file (locate-dominating-file default-directory file-name)))
    (list file-flag (concat (expand-file-name file) file-name))))

(defun apheleia-formatters-extension-p (&rest exts)
  "Assert whether current buffer has an extension in EXTS."
  (when-let ((name buffer-file-name)
             (ext (file-name-extension name)))
    (cl-find-if (apply-partially #'string-equal ext)
                exts)))

(defcustom apheleia-formatters-mode-extension-assoc
  '((c-mode . ".c")
    (c-ts-mode . ".c")
    (c++-mode . ".cpp")
    (c++-ts-mode . ".cpp")
    (glsl-mode . ".glsl")
    (java-mode . ".java")
    (java-ts-mode . ".java"))
  "Association list between major-modes and common file extensions for them."
  :type 'alist
  :group 'apheleia)

(defun apheleia-formatters-mode-extension (&optional flag)
  "Get a file-extension based on the current `major-mode'.
If FLAG is set this function returns a list of FLAG and then the extension.
Otherwise return the extension only."
  (when-let ((ext
              (alist-get major-mode apheleia-formatters-mode-extension-assoc)))
    (if flag
        (list flag ext)
      ext)))

(defun apheleia-formatters-local-buffer-file-name (&optional file-name)
  "Get FILE-NAME without any remote components.
FILE-NAME defaults to variable `buffer-file-name'."
  (when-let ((file-name (or file-name buffer-file-name)))
    (if-let ((remote (file-remote-p file-name)))
        (substring file-name (length remote))
      file-name)))

(provide 'apheleia-utils)

;;; apheleia-utils.el ends here
