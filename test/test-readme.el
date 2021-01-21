#!/usr/bin/env bash
:; exec emacs -Q -batch -script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'apheleia (expand-file-name "../apheleia.el"))
(require 'cl-lib)

(let ((mode-string
       (shell-command-to-string
        "sed -nE '/^\\| Mode\\s+\\|$/,/^$/p' ../README.md |
         tail -n+3 | head -n-1 | awk -F'|' '{print $2}' | cut -c2-"))
      (modes))
  (dolist (line (split-string mode-string "\n" t))
    (let* ((string-index (string-match
                          "\\(?:\\[?\\([[:alnum:]-]+\\)[][:space:]]\\)" line))
           (mo (match-string 1 line))
           (mode (assoc (intern mo) apheleia-mode-alist)))
      (unless mode
        (error "Mode not found in apheleia-mode-alist: %s" mo))
      (push mode modes)))
  (when-let (missing (cl-set-difference apheleia-mode-alist modes))
    (error "Modes absent from README: %s" (mapcar #'car missing))))

(let ((formatter-string
       (shell-command-to-string
        "sed -nE '/^\\| Formatter\\s+\\|.*\\|$/,/^$/p' ../README.md |
         tail -n+3 | head -n-1 | awk -F'|' '{print $2}' | cut -c2-"))
      (formatters))
  (dolist (line (split-string formatter-string "\n" t) formatters)
    (let* ((string-index (string-match "^\\[\\([[:alnum:]-]+\\)]" line))
           (form (match-string 1 line))
           (formatter (assoc (intern form) apheleia-formatters)))
      (unless formatter
        (error "Formatter not found in apheleia-formatters: %s" form))
      (push formatter formatters)))
  (when-let (missing (cl-set-difference apheleia-formatters formatters))
    (error "Formatters absent from README: %s" (mapcar #'car missing))))
