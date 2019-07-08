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

(defun apheleia--map-rcs-patch (func)
  "Map over the RCS patch in the current buffer.
For each RCS patch command, FUNC is called with an alist that has
the following keys:

- `command': either `addition' or `deletion'
- `start': line number, an integer
- `lines': number of lines to be inserted or removed
- `text': the string to be inserted, only for `addition'

See <https://tools.ietf.org/doc/tcllib/html/rcs.html#section4>
for documentation on the RCS patch format."
  (save-excursion
    (goto-char (point-min))
    (while (not (= (point) (point-max)))
      (unless (looking-at "$\\|\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
        (error "Malformed RCS patch: %S" (point)))
      (forward-line)
      (when-let ((command (match-string 1)))
        (let ((start (string-to-number (match-string 2)))
              (lines (string-to-number (match-string 3))))
          (pcase command
            ("a"
             (let ((text-start (point)))
               (forward-line lines)
               (funcall
                func
                `((command . addition)
                  (start . ,start)
                  (lines . ,lines)
                  (text . ,(buffer-substring-no-properties
                            text-start (point)))))))
            ("d"
             (funcall
              func
              `((command . deletion)
                (start . ,start)
                (lines . ,lines))))))))))

(defun apheleia--apply-rcs-patch (content-buffer patch-buffer)
  "Apply RCS patch.
CONTENT-BUFFER contains the text to be patched, and PATCH-BUFFER
contains the patch."
  (let ((commands nil)
        (point-list nil))
    (with-current-buffer content-buffer
      (push (cons nil (point)) point-list)
      (dolist (w (get-buffer-window-list nil nil t))
        (push (cons w (window-point w)) point-list)))
    (message "point-list: %S" point-list)
    (with-current-buffer patch-buffer
      (apheleia--map-rcs-patch
       (lambda (command)
         (with-current-buffer content-buffer
           ;; Could be optimized significantly by moving only as many
           ;; lines as needed, rather than returning to the beginning
           ;; of the buffer first.
           (save-excursion
             (goto-char (point-min))
             (forward-line (1- (alist-get 'start command)))
             ;; Account for the off-by-one error in the RCS patch spec
             ;; (namely, text is added *after* the line mentioned in
             ;; the patch).
             (when (eq (alist-get 'command command) 'addition)
               (forward-line))
             (push `(marker . ,(point-marker)) command)
             (push command commands)
             ;; If we delete a region just before inserting new text
             ;; at the same place, then it is a replacement. In this
             ;; case, check if the replaced region includes the window
             ;; point for any window currently displaying the content
             ;; buffer. If so, figure out where that window point
             ;; should be moved to, and record the information in an
             ;; additional command.
             ;;
             ;; See <https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Point.html>.
             ;;
             ;; Note that the commands get pushed in reverse order
             ;; because of how linked lists work.
             (let ((deletion (nth 1 commands))
                   (addition (nth 0 commands)))
               (when (and (eq (alist-get 'command deletion) 'deletion)
                          (eq (alist-get 'command addition) 'addition)
                          ;; Again with the weird off-by-one
                          ;; computations. For example, if you replace
                          ;; lines 68 through 71 inclusive, then the
                          ;; deletion is for line 68 and the addition
                          ;; is for line 70. Blame RCS.
                          (= (+ (alist-get 'start deletion)
                                (alist-get 'lines deletion)
                                -1)
                             (alist-get 'start addition)))
                 (let ((text-start (alist-get 'marker deletion)))
                   (forward-line (alist-get 'lines deletion))
                   (let ((text-end (point)))
                     (dolist (entry point-list)
                       ;; Check if the (window) point is within the
                       ;; replaced region.
                       (cl-destructuring-bind (w . p) entry
                         (when (and (< text-start p)
                                    (< p text-end))
                           (message "point %S for window %S is inside region" p w)
                           (let* ((old-text (buffer-substring-no-properties
                                             text-start text-end))
                                  (new-text (alist-get 'text addition))
                                  (old-relative-point (- p text-start))
                                  (new-relative-point
                                   (apheleia--align-point
                                    old-text new-text old-relative-point)))
                             (goto-char text-start)
                             (push `((marker . ,(point-marker))
                                     (command . set-point)
                                     (window . ,w)
                                     (relative-point . ,new-relative-point))
                                   commands))))))))))))))
    (with-current-buffer content-buffer
      (let ((move-to nil))
        (save-excursion
          (dolist (command (nreverse commands))
            (goto-char (alist-get 'marker command))
            (pcase (alist-get 'command command)
              (`addition
               (insert (alist-get 'text command)))
              (`deletion
               (let ((text-start (point)))
                 (forward-line (alist-get 'lines command))
                 (delete-region text-start (point))))
              (`set-point
               (let ((new-point
                      (+ (point) (alist-get 'relative-point command))))
                 (message "setting point for window %S to %S"
                          (alist-get 'window command)
                          new-point)
                 (if-let ((w (alist-get 'window command)))
                     (set-window-point w new-point)
                   (setq move-to new-point)))))))
        (when move-to
          (goto-char move-to))))))

(provide 'apheleia)

;; Local Variables:
;; outline-regexp: ";;;;* "
;; End:

;;; apheleia.el ends here
