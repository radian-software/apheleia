#!/usr/bin/env bash
:; exec emacs -Q -batch -script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

;;; Usage: test-apheleia.el [OPTIONS] FILE MODE FORMATTER
;;; FILE is a path to a file to be formatted
;;; MODE is the desired active major mode
;;; FORMATTER is the formatter to be run

(require 'apheleia (expand-file-name "../apheleia.el"))

(defun usage ()
  (with-temp-buffer
    (insert "Usage: test-apheleia.el FILE MODE FORMATTER\n\n"
            "FILE is a path to a file to be formatted\n"
            "MODE is the desired active major mode\n"
            "FORMATTER is the formatter to be run")
    (buffer-string)))

(defun get-file ()
  "Get and return FILE from command line arguments."
  (nth 1 argv))

(defun get-mode ()
  "Get and return MODE from command line arguments."
  (intern (nth 2 argv)))

(defun get-formatter ()
  "Get and return FORMATTER from command line arguments."
  (intern (nth 3 argv)))

(defun validate-args ()
  "Validate command line arguments."
  ;; First argument is always -- from the shebang.
  (unless (= (length argv) 4)
    (error (usage)))
  (let ((file (get-file))
        (mode (get-mode))
        (formatter (get-formatter)))
    (if (file-exists-p file)
        (unless (file-readable-p file)
          (error (format "Cannot read file: %s" file)))
      (error (format "File not found: %s" file)))
    (unless (assoc mode apheleia-mode-alist)
      (error (format "Mode not found in apheleia-mode-alist: %s"
                     (symbol-name mode))))
    (unless (assoc formatter apheleia-formatters)
      (error (format "Formatter not found in apheleia-formatters: %s"
                     (symbol-name formatter))))))

(validate-args)

(let* ((file (get-file))
       (mode (get-mode))
       (formatter (get-formatter))
       (proc
        (with-current-buffer (find-file-noselect file)
          (apheleia--run-formatter
           (alist-get (alist-get mode apheleia-mode-alist) apheleia-formatters)
           (lambda (buf) (with-current-buffer buf (princ (buffer-string))))))))
  (while (accept-process-output proc)))
