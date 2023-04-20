;;; apheleia-utils.el --- Formatter helpers. -*- lexical-binding: t -*-

;;; Commentary:

;; Helper functions for defining apheleia formatters.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun apheleia-formatters-indent (tab-flag indent-flag indent-var)
  "Set flag for indentation.
Helper function for `apheleia-formatters' which allows you to supply
alternating flags based on the current buffers indent configuration. If the
buffer is indented with tabs then returns TAB-FLAG. Otherwise if INDENT-VAR
is set in the buffer return INDENT-FLAG and the value of INDENT-VAR. Use this
to easily configure the indentation level of a formatter."
  (cond
   (indent-tabs-mode tab-flag)
   (indent-var
    (when-let ((indent (and (boundp indent-var)
                            (symbol-value indent-var))))
      (list indent-flag (number-to-string indent))))))

(defun apheleia-formatters-js-indent (tab-flag indent-flag)
  "Variant of `apheleia-formatters-indent' for JavaScript like modes.
See `apheleia-formatters-indent' for a description of TAB-FLAG and
INDENT-FLAG."
  (apheleia-formatters-indent
   tab-flag indent-flag
   (cl-case major-mode
     (json-mode 'js-indent-level)
     (json-ts-mode 'json-ts-mode-indent-offset)
     (js-mode 'js-indent-level)
     (js-jsx-mode 'js-indent-level)
     (js2-mode 'js2-basic-offset)
     (js2-jsx-mode 'js2-basic-offset)
     (js3-mode 'js3-indent-level))))

(defcustom apheleia-formatters-respect-fill-column nil
  "Whether formatters should set `fill-column' related flags."
  :type 'boolean
  :group 'apheleia)

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

(defun apheleia-formatters-local-buffer-file-name ()
  "Get variable `buffer-file-name' without any remote components."
  (when-let ((name buffer-file-name))
    (let ((remote (file-remote-p name)))
      (if remote
          (substring name (length remote))
        name))))

(provide 'apheleia-utils)

;;; apheleia-utils.el ends here
