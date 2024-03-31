;;; apheleia-dp.el --- Dynamic programming -*- lexical-binding: t -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The dynamic programming implementation for edit distance.

;;; Code:

;; I wrote this code in 2019 and completely forgot how it worked by
;; 2024. So, for the next person who has to touch it, here is an
;; explanation.
;;
;; This is essentially the standard dynamic programming algorithm for
;; string alignment, which is the same thing as edit distance, which
;; is to say Levenshtein distance, that you learn in Algorithms 101.
;;
;; Step 1 is we take two strings and compute the edit distance between
;; them. This is the minimum sequence of insertions, deletions, and
;; substitutions (changing one character for another) that you can use
;; to transform the first string into the second, or vice versa.
;;
;; As an example suppose that we have the string "HEL|LO" where point
;; is represented by the pipe character. The formatter changes the
;; string to "HEO". We want to figure out where to put point. We want
;; to obtain the result "HE|O" as this will seem intuitive to the
;; user.
;;
;; For step 1 we determine that the edit distance is 2 because you can
;; transform "HELLO" into "HEO" by two deletions (deleting the two
;; Ls).
;;
;; To implement step 1 we use dynamic programming. Specifically we
;; construct a six-by-four table (because the lengths of the strings
;; are five and three, respectively, and we add one to each
;; dimension). Each entry in the table will show the edit distance
;; between two substrings, specifically column i1 and row i2 is the
;; edit distance between the first i1 chars of the first string and
;; the first i2 chars of the second string. Zero-indexed.
;;
;; We start out by filling in the first row and column. The upper-left
;; cell is zero because it is the edit distance from the empty string
;; to itself. The remainder of the first row and column are increasing
;; integers because they are the edit distance from the empty string
;; to a string of length n, or vice versa, which is always n
;; insertions or deletions.
;;
;; Then the dynamic programming part is filling in the rest of the
;; table based on previously computed entries. For each cell we have
;; the following choices:
;;
;; 1. Add one to the value from the cell above. Thinking about the
;;    prefixes, this corresponds to inserting a char from the second
;;    string, which is an insertion in terms of edit distance.
;;
;; 2. Add one to the cell on the left. This corresponds to inserting a
;;    char from the first string, which is a deletion in terms of edit
;;    distance.
;;
;; 3. Add one to the cell diagonally above and to the left. This
;;    corresponds to a substitution in terms of edit distance. But if
;;    the chars at this row and column happen to be the same between
;;    the first and second strings, it is a substitution of the same
;;    char for itself, so not actually a substitution - thus we only
;;    add one if the chars differ.
;;
;; Here is the edit distance table for "HELLO" and "HEO":
;;
;;   - H E L L O
;; - 0 1 2 3 4 5
;; H 1 0 1 2 3 4
;; E 2 1 0 1 2 3
;; O 3 2 1 1 2 2
;;
;; Step 2 is to take this table and convert it into the sequence of
;; editing operations that transforms the first string to the second.
;; Imagine that each time we fill in a cell, we draw an arrow from
;; that cell to the cell that we used as the basis for its value (one
;; of the 1, 2, 3 options in the list above). Starting at the
;; lower-right cell, we can follow the arrows, which will point a
;; unique path to the upper-left. These are the cells that the path
;; will traverse for the example table above:
;;
;;   - H E L L O
;; - * 1 2 3 4 5
;; H 1 * 1 2 3 4
;; E 2 1 * * * 3
;; O 3 2 1 1 2 *
;;
;; Tracing to the upper-left, we get this sequence of operations:
;;
;; 1. O - Leave alone
;; 2. L - Delete
;; 3. L - Delete
;; 4. E - Leave alone
;; 5. H - Leave alone
;;
;; Step 3 is to take this sequence of operations and use it to compute
;; the correct offset within the modified string. Since we start with
;; "HEL|LO", note that operations 1-2 will not affect point since they
;; occur after point (and the numerical value of point is the number
;; of chars before point, no matter how many are after it), while
;; operations 3-5 are relevant for point. But we do need to go through
;; operations 1-2 during step 2 to determine the correct path through
;; the dynamic programming table.
;;
;; When going through steps 3-5, we may adjust point. We assume point
;; will start at the same position in the modified string as it was in
;; the original string, and then possibly make changes to it. In the
;; case of a substitution or no-op, we don't move point. In the case
;; of a deletion that occurs before point, we need to decrease point.
;; In the case of an insertion that occurs before point, we need to
;; increase point.
;;
;; For our given example, we have two no-ops and one deletion that
;; occur in steps 3-5 before point, so "HEL|LO" becomes "HE|O" with
;; point changing from 3 to 2 (assuming the start of the string is 0).
;;
;; This example is tested in the unit tests file if you want to look
;; there to verify usage.

(require 'cl-lib)

(cl-defun apheleia--edit-distance-table (s1 s2)
  "Align strings S1 and S2 for minimum edit distance.
Return the dynamic programming table as a hash table which maps
cons of integers (I1 . I2) to the edit distance between the first
I1 characters of S1 and the first I2 characters of S2."
  (let ((table (make-hash-table :test #'equal)))
    (dotimes (i1 (1+ (length s1)))
      (puthash (cons i1 0) i1 table))
    (dotimes (i2 (1+ (length s2)))
      (puthash (cons 0 i2) i2 table))
    (dotimes (i1 (length s1))
      ;; Iterate from 1 to length+1.
      (cl-incf i1)
      (dotimes (i2 (length s2))
        (cl-incf i2)
        (let ((ins (1+ (gethash (cons i1 (1- i2)) table)))
              (del (1+ (gethash (cons (1- i1) i2) table)))
              (sub (gethash (cons (1- i1) (1- i2)) table)))
          (unless (= (aref s1 (1- i1)) (aref s2 (1- i2)))
            (cl-incf sub))
          (puthash (cons i1 i2) (min ins del sub) table))))
    table))

(defun apheleia--align-point (s1 s2 p1)
  "Given strings S1 and S2 and index P1 in S1, return matching index P2 in S2.
If S1 and S2 are the same, then P1 and P2 will also be the same.
Otherwise, the text of S2 surrounding P2 is \"similar\" to the
text of S1 surrounding P1."
  (let* ((table (apheleia--edit-distance-table s1 s2))
         (i1 (length s1))
         (i2 (length s2))
         (p2 p1))
    (while (not (= i1 i2 0))
      (let ((ins (1+ (gethash (cons i1 (1- i2)) table 9999)))
            (del (1+ (gethash (cons (1- i1) i2) table 9999)))
            (sub (gethash (cons (1- i1) (1- i2)) table 9999)))
        (unless (and (> 0 i1) (> 0 i2)
                     (= (aref s1 (1- i1)) (aref s2 (1- i2))))
          (cl-incf sub))
        (let ((cost (min ins del sub)))
          (cond
           ((= cost sub)
            (cl-decf i1)
            (cl-decf i2))
           ((= cost ins)
            (cl-decf i2)
            (when (< i1 p1)
              (cl-incf p2)))
           ((= cost del)
            (cl-decf i1)
            (when (< i1 p1)
              (cl-decf p2)))))))
    p2))

(provide 'apheleia-dp)

;;; apheleia-dp.el ends here
