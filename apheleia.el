;;; apheleia.el --- Reformat buffer stably -*- lexical-binding: t -*-

;; Copyright (C) 2019 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 7 Jul 2019
;; Homepage: https://github.com/raxod502/apheleia
;; Keywords: tools
;; Package-Requires: ((emacs "25.2"))
;; Version: 0

;;; Commentary:

;; Apheleia is an Emacs Lisp package which allows you to reformat a
;; buffer without moving point. This solves the usual problem of
;; running a tool like Prettier or Black on `before-save-hook', namely
;; that it resets point to the beginning of the buffer. Apheleia
;; maintains the position of point relative to its surrounding text
;; even if the buffer is modified by the reformatting.

;; Please see https://github.com/raxod502/apheleia for more information.

;;; Code:

(require 'cl-lib)

(defgroup apheleia nil
  "Better mode lighter overriding."
  :group 'external
  :link '(url-link :tag "GitHub" "https://github.com/raxod502/apheleia")
  :link '(emacs-commentary-link :tag "Commentary" "apheleia"))

(cl-defun apheleia--align-strings (s1 s2 &key i1 i2 memo)
  "Align strings S1 and S2 for minimum edit distance.
Return a hash table mapping (I1 . I2) to COST, where I1 and I2
are indices into S1 and S2, and COST is the edit distance between
the substrings of S1 and S2 starting at I1 and I2 respectively.

Recursively, I1 and I2 are indices as explained above, and MEMO
is the hash table being filled."
  (if (null memo)
      (let ((memo (make-hash-table :test #'equal)))
        (prog1 memo
          (apheleia--align-strings s1 s2 :i1 0 :i2 0 :memo memo)))
    (or
     (gethash (cons i1 i2) memo)
     (puthash
      (cons i1 i2)
      (cond
       ((= i1 (length s1))
        (- (length s2) i2))
       ((= i2 (length s2))
        (- (length s1) i1))
       ((= (aref s1 i1) (aref s2 i2))
        (apheleia--align-strings s1 s2 :i1 (1+ i1) :i2 (1+ i2) :memo memo))
       (t
        (1+
         (min
          (apheleia--align-strings s1 s2 :i1 (1+ i1) :i2 (1+ i2) :memo memo)
          (apheleia--align-strings s1 s2 :i1     i1  :i2 (1+ i2) :memo memo)
          (apheleia--align-strings s1 s2 :i1 (1+ i1) :i2     i2  :memo memo)))))
      memo))))

(defun apheleia--align-point (s1 s2 p1)
  "Given strings S1 and S2 and index P1 in S1, return matching index P2 in S2.
If S1 and S2 are the same, then P1 and P2 will also be the same.
Otherwise, the text of S2 surrounding P2 is \"similar\" to the
text of S1 surrounding P1."
  (let* ((memo (apheleia--align-strings s1 s2))
         (i1 0)
         (i2 0))
    (while (< i1 p1)
      (message "currently at %S, %S" i1 i2)
      (let* ((costs
              `(,(gethash (cons (1+ i1) (1+ i2)) memo)
                ;; Replicate the short-circuiting in our dynamic
                ;; programming implementation; otherwise we will be
                ;; trying to look up hash table entries that don't
                ;; exist.
                ,@(unless (= (aref s1 i1) (aref s2 i2))
                    (list
                     (gethash (cons     i1  (1+ i2)) memo)
                     (gethash (cons (1+ i1)     i2)  memo)))))
             (min-cost (apply #'min costs)))
        (cond
         ((= min-cost (nth 0 costs))
          (cl-incf i1)
          (cl-incf i2))
         ((= min-cost (nth 1 costs))
          (cl-incf i2))
         ((= min-cost (nth 2 costs))
          (cl-incf i1)))))
    i2))

(provide 'apheleia)

;; Local Variables:
;; outline-regexp: ";;;;* "
;; End:

;;; apheleia.el ends here
